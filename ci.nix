# Should work with any nixpkgs revision, so we don't care
{ pkgs ? import <nixpkgs> { } }:

let
  inherit (pkgs) lib haskell;
  inherit (haskell.lib.compose) buildFromSdist overrideCabal;

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
      (hpkgs.callCabal2nix "jailbreak-cabal-ghc-${hpkgs.ghc.version}.nix" ./. {})
      [
        # Check for packaging bugs
        buildFromSdist
        # To better identify build failures
        (overrideCabal { pname = "jailbreak-cabal-ghc-${hpkgs.ghc.version}"; })
      ])
  eligible
