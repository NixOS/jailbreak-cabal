# Should work with any nixpkgs revision, so we don't care
{ pkgs ? import <nixpkgs> { } }:

let
  inherit (pkgs) lib haskell;
  inherit (haskell.lib.compose) buildFromSdist overrideCabal;

  expr =
    { mkDerivation, base, Cabal, Cabal-syntax, lib }:
    mkDerivation {
      pname = "jailbreak-cabal";
      version = "unstable-unknown";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = [ base Cabal Cabal-syntax ];
      homepage = "https://github.com/NixOS/jailbreak-cabal";
      description = "Strip version restrictions from Cabal files";
      license = lib.licenses.bsd3;
      mainProgram = "jailbreak-cabal";
    };

  # All non-binary GHC package sets
  eligible = lib.filterAttrs
    (name: _:
      !(lib.hasInfix "Binary" name)
      && !(lib.hasPrefix "ghcjs" name)
      && lib.hasPrefix "ghc" name)
    haskell.packages;
in

lib.mapAttrs
  (_: hpkgs:
    # cabal-install is not cached for all package sets, so we'll rely on Setup.hs
    lib.pipe
      (hpkgs.callPackage expr { })
      [
        # Check for packaging bugs
        buildFromSdist
        # To better identify build failures
        (overrideCabal (old: { version = "${old.version}+ghc-${hpkgs.ghc.version}"; }))
      ])
  eligible
