{
  inputs = {
    git-ignore-nix.url = "github:hercules-ci/gitignore.nix/master";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, git-ignore-nix }:
    let
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
            (hself: hsuper: {
              xmobar = prev.haskell.lib.compose.dontCheck (hself.callCabal2nix "xmobar"
                (git-ignore-nix.lib.gitignoreSource ./.) { });
            });
        });
      };
      overlays = [ overlay ];
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system overlays; };
          dynamicLibraries = with pkgs; [
            xorg.libX11
            xorg.libXrandr
            xorg.libXrender
            xorg.libXScrnSaver
            xorg.libXext
            xorg.libXft
            xorg.libXpm.out
            xorg.libXrandr
            xorg.libXrender
          ];
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.xmobar ];
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            #haskellPackages.haskell-language-server
          ] ++ dynamicLibraries;

          LD_LIBRARY_PATH = pkgs.lib.strings.makeLibraryPath dynamicLibraries;
        };
        defaultPackage = pkgs.haskellPackages.xmobar;
      }) // {
        inherit overlay overlays;
      };
}
