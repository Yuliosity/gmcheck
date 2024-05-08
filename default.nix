{ system ? builtins.currentSystem or "x86_64-linux", ghc ? "ghc947",
  withProfiling ? false }:

let
  nix = import ./nix;
  dontCheck = drv: drv.overrideAttrs(
    oa: { checkPhase = "" ; installCheckPhase = ""; doInstallCheck = false; doCheck = false; dontCheck = true; });

  pkgs = nix.pkgSetForSystem system {
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
    overlays = [];
  };

  inherit (pkgs) lib;

  inherit (pkgs.haskell.lib)
    enableSharedExecutables
    disableLibraryProfiling disableExecutableProfiling
    enableLibraryProfiling enableExecutableProfiling ;

  controlProfiling = drv:
    if withProfiling then
      builtins.trace "profiling is ON"
        (enableLibraryProfiling (enableExecutableProfiling drv))
    else
      disableLibraryProfiling (disableExecutableProfiling drv);

  haskellPkgSetOverlay = pkgs.callPackage ./nix/haskell/overlay.nix {
    inherit (nix) sources;
  };

  sourceRegexes = [
    "^src.*$"
    "^app.*$"
    "^package.yaml"
    "^.*\\.cabal$"
    "^LICENSE$"
  ];

  base = enableSharedExecutables (haskellPkgs.callCabal2nix "gmcheck" (lib.sourceByRegex ./. sourceRegexes) {});
  overlay = _hfinal: _hprev:
    { gmcheck = base.overrideAttrs(oa: {});
    };
  baseHaskellPkgs = pkgs.haskell.packages.${ghc};
  haskellOverlays = [ haskellPkgSetOverlay overlay ];
  haskellPkgs = baseHaskellPkgs.override (old: {
    overrides =
      builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      haskellOverlays;
  });

  haskellLanguageServer =
    pkgs.haskell.lib.overrideCabal haskellPkgs.haskell-language-server
    (_: { enableSharedExecutables = true; });

  shell = haskellPkgs.shellFor {
    packages = p: [ p.gmcheck ];

    shellHook = ''echo hook'';
    nativeBuildInputs = [ haskellLanguageServer ] ++ (with pkgs; [
      cabal-install
      ghcid
      hlint
      niv
      llvm
      hpack
      glibcLocales
    ]);
  };

  gmcheck = haskellPkgs.gmcheck;
in {
  inherit haskellPkgs;
  inherit ghc;
  inherit pkgs;
  inherit shell;
  inherit gmcheck;
  inherit haskellOverlays;
  inherit haskellLanguageServer;
}
