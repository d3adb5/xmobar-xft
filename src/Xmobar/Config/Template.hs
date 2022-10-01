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

import Control.Monad (guard, mzero)
import Data.Maybe (fromMaybe)

import Text.Parsec ((<|>))
import Text.Read (readMaybe)

import qualified Text.Parsec as P
import qualified Text.Parsec.Combinator as C

import Text.ParserCombinators.Parsec (Parser)

import Xmobar.Config.Types

-- | Runs the template string parser
parseString :: Config -> String -> [Segment]
parseString c s =
    case P.parse (stringParser ci 0 Nothing) "" s of
      Left  _ -> [(Text $ "Could not parse string: " ++ s , ci , 0 , Nothing)]
      Right x -> concat x
    where ci = TextRenderInfo (fgColor c) 0 0 []

allParsers :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
allParsers c f a =  textParser c f a
                <|> P.try (iconParser c f a)
                <|> P.try (hspaceParser c f a)
                <|> P.try (rawParser c f a)
                <|> P.try (actionParser c f a)
                <|> P.try (fontParser c a)
                <|> P.try (boxParser c f a)
                <|> colorParser c f a

-- | Gets the string and combines the needed parsers
stringParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [[Segment]]
stringParser c f a = C.manyTill (allParsers c f a) C.eof

-- | Parses a maximal string without markup.
textParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
textParser c f a = do s <- C.many1 $
                            P.noneOf "<" <|>
                              P.try (notFollowedBy' (P.char '<')
                                    (P.try (P.string "fc=")  <|>
                                     P.try (P.string "box")  <|>
                                     P.try (P.string "fn=")  <|>
                                     P.try (P.string "action=") <|>
                                     P.try (P.string "/action>") <|>
                                     P.try (P.string "icon=") <|>
                                     P.try (P.string "hspace=") <|>
                                     P.try (P.string "raw=") <|>
                                     P.try (P.string "/fn>") <|>
                                     P.try (P.string "/box>") <|>
                                     P.string "/fc>"))
                      return [(Text s, c, f, a)]

-- | Parse a "raw" tag, which we use to prevent other tags from creeping in.
-- The format here is net-string-esque: a literal "<raw=" followed by a
-- string of digits (base 10) denoting the length of the raw string,
-- a literal ":" as digit-string-terminator, the raw string itself, and
-- then a literal "/>".
rawParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
rawParser c f a = do
  P.string "<raw="
  lenstr <- C.many1 P.digit
  P.char ':'
  case reads lenstr of
    [(len,[])] -> do
      guard ((len :: Integer) <= fromIntegral (maxBound :: Int))
      s <- C.count (fromIntegral len) P.anyChar
      P.string "/>"
      return [(Text s, c, f, a)]
    _ -> mzero

-- | Wrapper for notFollowedBy that returns the result of the first parser.
--   Also works around the issue that, at least in Parsec 3.0.0, notFollowedBy
--   accepts only parsers with return type Char.
notFollowedBy' :: Parser a -> Parser b -> Parser a
notFollowedBy' p e = do x <- p
                        C.notFollowedBy $ P.try (e >> return '*')
                        return x

iconParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
iconParser c f a = do
  P.string "<icon="
  i <- C.manyTill (P.noneOf ">") (P.try (P.string "/>"))
  return [(Icon i, c, f, a)]

hspaceParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
hspaceParser c f a = do
  P.string "<hspace="
  pVal <- C.manyTill P.digit (P.try (P.string "/>"))
  return [(Hspace (fromMaybe 0 $ readMaybe pVal), c, f, a)]

actionParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
actionParser c f act = do
  P.string "<action="
  command <- C.choice [C.between (P.char '`') (P.char '`') (C.many1 (P.noneOf "`")),
                   C.many1 (P.noneOf ">")]
  buttons <- (P.char '>' >> return "1") <|> (P.space >> P.spaces >>
    C.between (P.string "button=") (P.string ">") (C.many1 (P.oneOf "12345")))
  let a = Spawn (toButtons buttons) command
      a' = case act of
        Nothing -> Just [a]
        Just act' -> Just $ a : act'
  s <- C.manyTill (allParsers c f a') (P.try $ P.string "</action>")
  return (concat s)

toButtons :: String -> [Button]
toButtons = map (\x -> read [x])

-- | Parsers a string wrapped in a color specification.
colorParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
colorParser (TextRenderInfo _ _ _ bs) fidx a = do
  c <- C.between (P.string "<fc=") (P.string ">") colors
  let colorParts = break (==':') c
  let (ot,ob) = case break (==',') (Prelude.drop 1 $ snd colorParts) of
                  (top,',':btm) -> (top, btm)
                  (top,      _) -> (top, top)
      tri = TextRenderInfo (fst colorParts)
                           (fromMaybe (-1) $ readMaybe ot)
                           (fromMaybe (-1) $ readMaybe ob)
                           bs
  s <- C.manyTill (allParsers tri fidx a) (P.try $ P.string "</fc>")
  return (concat s)

-- | Parses a string wrapped in a box specification.
boxParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
boxParser (TextRenderInfo cs ot ob bs) f a = do
  c <- C.between (P.string "<box") (P.string ">")
               (C.option "" (C.many1 (P.alphaNum
                                  <|> P.char '='
                                  <|> P.char ' '
                                  <|> P.char '#'
                                  <|> P.char ',')))
  let b = Box BBFull (BoxOffset C 0) 1 cs (BoxMargins 0 0 0 0)
  let g = boxReader b (words c)
  s <- C.manyTill
       (allParsers (TextRenderInfo cs ot ob (g : bs)) f a)
       (P.try $ P.string "</box>")
  return (concat s)

boxReader :: Box -> [String] -> Box
boxReader b [] = b
boxReader b (x:xs) = do
  let (param,val) = case break (=='=') x of
                 (p,'=':v) -> (p, v)
                 (p,    _) -> (p, "")
  boxReader (boxParamReader b param val) xs

boxParamReader :: Box -> String -> String -> Box
boxParamReader b _ "" = b
boxParamReader (Box bb off lw fc mgs) "type" val =
  Box (fromMaybe bb $ readMaybe ("BB" ++ val)) off lw fc mgs
boxParamReader (Box bb (BoxOffset alg off) lw fc mgs) "offset" (a:o) =
  Box bb (BoxOffset align offset) lw fc mgs
  where offset = fromMaybe off $ readMaybe o
        align = fromMaybe alg $ readMaybe [a]
boxParamReader (Box bb off lw fc mgs) "width" val =
  Box bb off (fromMaybe lw $ readMaybe val) fc mgs
boxParamReader (Box bb off lw _ mgs) "color" val =
  Box bb off lw val mgs
boxParamReader (Box bb off lw fc mgs@(BoxMargins mt mr mb ml)) ('m':pos) val = do
  let mgs' = case pos of
         "t" -> BoxMargins (fromMaybe mt $ readMaybe val) mr mb ml
         "r" -> BoxMargins mt (fromMaybe mr $ readMaybe val) mb ml
         "b" -> BoxMargins mt mr (fromMaybe mb $ readMaybe val) ml
         "l" -> BoxMargins mt mr mb (fromMaybe ml $ readMaybe val)
         _ -> mgs
  Box bb off lw fc mgs'
boxParamReader b _ _ = b

-- | Parsers a string wrapped in a font specification.
fontParser :: TextRenderInfo -> Maybe [Action] -> Parser [Segment]
fontParser c a = do
  f <- C.between (P.string "<fn=") (P.string ">") colors
  s <- C.manyTill (allParsers c (fromMaybe 0 $ readMaybe f) a) (P.try $ P.string "</fn>")
  return (concat s)

-- | Parses a color specification (hex or named)
colors :: Parser String
colors = C.many1 (P.alphaNum <|> P.char ',' <|> P.char ':' <|> P.char '#')
