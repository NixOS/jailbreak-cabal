# Should work with any nixpkgs revision, so we don't care
{ pkgs ? import <nixpkgs> { } }:

let
  inherit (pkgs) lib haskell;
  inherit (haskell.lib.compose) buildFromSdist overrideCabal;

  src = builtins.path {
    name = "jailbreak-cabal-source";
    path = ./.;
  };

  # Insert a bogus constraint into jailbreak-cabal's cabal file
  # and immediately remove it using a given version of jailbreak-cabal
  fixedBrokenSrc = jailbreak-cabal: pkgs.stdenvNoCC.mkDerivation {
    name = "fixed-broken-jailbreak-cabal-source";

    inherit src;
    nativeBuildInputs = [ jailbreak-cabal ];

    dontConfigure = true;

    postPatch = ''
      cp *.cabal backup

      substituteInPlace *.cabal \
        --replace-fail 'base < 5' 'base < 1'

      diff -u backup *.cabal || true
    '';

    buildPhase = ''
      jailbreak-cabal *.cabal
      diff -u backup *.cabal || true
    '';

    installPhase = ''
      cp -r . "$out"
    '';
  };

  expr =
    { mkDerivation, base, Cabal, Cabal-syntax, lib }:
    mkDerivation {
      pname = "jailbreak-cabal";
      version = "unstable-unknown";
      inherit src;
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

        # Rebuild jailbreak-cabal after inserting some broken constraints and removing it
        # using the jailbreak-cabal we've just built.
        (prev-jailbreak-cabal: haskell.lib.compose.overrideCabal (old: {
          pname = "${old.pname}-from-broken-source";
          src = fixedBrokenSrc prev-jailbreak-cabal;
        }) prev-jailbreak-cabal)
      ])
  eligible
