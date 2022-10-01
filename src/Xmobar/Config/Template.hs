------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Config.Template
-- Copyright: (c) 2022 jao
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: mail@jao.io
-- Stability: unstable
-- Portability: portable
-- Created: Fri Sep 30, 2022 06:33
--
--
-- Parsing template strings
--
------------------------------------------------------------------------------


module Xmobar.Config.Template (parseString) where

import Data.Maybe (fromMaybe)
import qualified Control.Monad as CM

import Text.Parsec ((<|>))
import Text.Read (readMaybe)

import qualified Text.Parsec as P
import qualified Text.Parsec.Combinator as C

import Text.ParserCombinators.Parsec (Parser)

import qualified Xmobar.Config.Types as T

type Context = (T.TextRenderInfo, T.FontIndex, Maybe [T.Action])

retSegment :: Context -> T.Widget -> Parser [T.Segment]
retSegment (i, idx, as) widget = return [(widget, i, idx, as)]

-- | Run the template string parser for the given config, producing a list of
-- drawable segment specifications.
parseString :: T.Config -> String -> [T.Segment]
parseString c s =
    case P.parse (stringParser ci) "" s of
      Left  _ -> [(T.Text $ "Could not parse string: " ++ s, ti, 0, Nothing)]
      Right x -> concat x
    where ci = (ti , 0, Nothing)
          ti = T.TextRenderInfo (T.fgColor c) 0 0 []

-- Top level parser reading the full template string
stringParser :: Context -> Parser [[T.Segment]]
stringParser c = C.manyTill (allParsers c) C.eof

allParsers :: Context -> Parser [T.Segment]
allParsers c = C.choice (textParser c:map (\f -> P.try (f c)) parsers)
  where parsers = [ iconParser, hspaceParser, rawParser, actionParser
                  , fontParser, boxParser, colorParser ]

-- Wrapper for notFollowedBy that returns the result of the first parser.
-- Also works around the issue that, at least in Parsec 3.0.0, notFollowedBy
-- accepts only parsers with return type Char.
notFollowedBy' :: Parser a -> Parser b -> Parser a
notFollowedBy' p e = do x <- p
                        C.notFollowedBy $ P.try (e >> return '*')
                        return x

-- Parse a maximal string without markup
textParser :: Context -> Parser [T.Segment]
textParser c =
  C.many1 (P.noneOf "<" <|> P.try (notFollowedBy' (P.char '<') suffixes))
  >>= retSegment c . T.Text
  where suffixes = C.choice $  map (P.try . P.string)
                   [ "icon=" , "hspace=", "raw="
                   , "action=", "/action>", "fn=", "/fn>"
                   , "box", "/box>", "fc=", "/fc>" ]

-- Parse a "raw" tag, which we use to prevent other tags from creeping in.
-- The format here is net-string-esque: a literal "<raw=" followed by a string
-- of digits (base 10) denoting the length of the raw string, a literal ":" as
-- digit-string-terminator, the raw string itself, and then a literal "/>".
rawParser :: Context -> Parser [T.Segment]
rawParser c = do
  P.string "<raw="
  lenstr <- C.many1 P.digit
  P.char ':'
  case reads lenstr of
    [(len,[])] -> do
      CM.guard ((len :: Integer) <= fromIntegral (maxBound :: Int))
      s <- C.count (fromIntegral len) P.anyChar
      P.string "/>"
      retSegment c (T.Text s)
    _ -> CM.mzero

iconParser :: Context -> Parser [T.Segment]
iconParser c = do
  P.string "<icon="
  i <- C.manyTill (P.noneOf ">") (P.try (P.string "/>"))
  retSegment c (T.Icon i)

hspaceParser :: Context -> Parser [T.Segment]
hspaceParser c = do
  P.string "<hspace="
  pVal <- C.manyTill P.digit (P.try (P.string "/>"))
  retSegment c (T.Hspace (fromMaybe 0 $ readMaybe pVal))

actionParser :: Context -> Parser [T.Segment]
actionParser (ti, fi, act) = do
  P.string "<action="
  command <- C.choice [C.between (P.char '`') (P.char '`') (C.many1 (P.noneOf "`"))
                      , C.many1 (P.noneOf ">")]
  buttons <- (P.char '>' >> return "1") <|> (P.space >> P.spaces >>
    C.between (P.string "button=") (P.string ">") (C.many1 (P.oneOf "12345")))
  let a = T.Spawn (toButtons buttons) command
      a' = case act of
        Nothing -> Just [a]
        Just act' -> Just $ a : act'
  s <- C.manyTill (allParsers (ti, fi, a')) (P.try $ P.string "</action>")
  return (concat s)

toButtons :: String -> [T.Button]
toButtons = map (\x -> read [x])

colorParser :: Context -> Parser [T.Segment]
colorParser (T.TextRenderInfo _ _ _ bs, fidx, a) = do
  c <- C.between (P.string "<fc=") (P.string ">") (C.many1 colorc)
  let colorParts = break (==':') c
  let (ot,ob) = case break (==',') (drop 1 $ snd colorParts) of
                  (top,',':btm) -> (top, btm)
                  (top,      _) -> (top, top)
      tri = T.TextRenderInfo (fst colorParts)
                           (fromMaybe (-1) $ readMaybe ot)
                           (fromMaybe (-1) $ readMaybe ob)
                           bs
  s <- C.manyTill (allParsers (tri, fidx, a)) (P.try $ P.string "</fc>")
  return (concat s)
  where colorc = P.alphaNum <|> P.oneOf ",:#"

boxParser :: Context -> Parser [T.Segment]
boxParser (T.TextRenderInfo cs ot ob bs, f, a) = do
  c <- C.between (P.string "<box") (P.string ">")
                 (C.option "" (C.many1 (P.alphaNum <|> P.oneOf "= #,")))
  let b = T.Box T.BBFull (T.BoxOffset T.C 0) 1 cs (T.BoxMargins 0 0 0 0)
  let g = boxReader b (words c)
  s <- C.manyTill
       (allParsers (T.TextRenderInfo cs ot ob (g : bs), f, a))
       (P.try $ P.string "</box>")
  return (concat s)

boxReader :: T.Box -> [String] -> T.Box
boxReader b [] = b
boxReader b (x:xs) = boxReader (boxParamReader b param val) xs
  where (param,val) = case break (=='=') x of
                        (p,'=':v) -> (p, v)
                        (p,    _) -> (p, "")

boxParamReader :: T.Box -> String -> String -> T.Box
boxParamReader b _ "" = b

boxParamReader (T.Box bb off lw fc mgs) "type" val =
  T.Box (fromMaybe bb $ readMaybe ("BB" ++ val)) off lw fc mgs

boxParamReader (T.Box bb (T.BoxOffset alg off) lw fc mgs) "offset" (a:o) =
  T.Box bb (T.BoxOffset align offset) lw fc mgs
  where offset = fromMaybe off $ readMaybe o
        align = fromMaybe alg $ readMaybe [a]

boxParamReader (T.Box bb off lw fc mgs) "width" val =
  T.Box bb off (fromMaybe lw $ readMaybe val) fc mgs

boxParamReader (T.Box bb off lw _ mgs) "color" val =
  T.Box bb off lw val mgs

boxParamReader (T.Box bb off lw fc mgs@(T.BoxMargins mt mr mb ml)) ('m':pos) v =
  let mgs' = case pos of
               "t" -> T.BoxMargins (maybeVal mt) mr mb ml
               "r" -> T.BoxMargins mt (maybeVal mr) mb ml
               "b" -> T.BoxMargins mt mr (maybeVal mb) ml
               "l" -> T.BoxMargins mt mr mb (maybeVal ml)
               _   -> mgs
      maybeVal d = fromMaybe d (readMaybe v)
  in T.Box bb off lw fc mgs'

boxParamReader b _ _ = b

fontParser :: Context -> Parser [T.Segment]
fontParser (i, _, a) = do
  f <- C.between (P.string "<fn=") (P.string ">") (C.many1 P.digit)
  s <- C.manyTill (allParsers (i, fromMaybe 0 $ readMaybe f, a))
                  (P.try $ P.string "</fn>")
  return (concat s)
