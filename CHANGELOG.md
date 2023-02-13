# Revision history for jailbreak-cabal

## 1.4

* jailbreak-cabal will now also relax version constraints on `build-tool-depends`.
  See [#20](https://github.com/NixOS/jailbreak-cabal/pull/20).

* Introduced new (automatic) cabal flag `Cabal-syntax` which should prevent
  Cabal's constraint solver from picking incompatible versions of `Cabal` and
  `Cabal-syntax` when using `cabal-install` to build jailbreak-cabal.
  See [#22](https://github.com/NixOS/jailbreak-cabal/pull/22).
