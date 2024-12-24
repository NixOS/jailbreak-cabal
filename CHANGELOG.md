# Revision history for jailbreak-cabal

## 1.4.1

* Support building with `Cabal >= 3.14`. Adjustment for API changes that should not
  change behavior. See [#25](https://github.com/NixOS/jailbreak-cabal/pull/25).

## 1.4

* jailbreak-cabal will now also relax version constraints on `build-tool-depends`.
  See [#20](https://github.com/NixOS/jailbreak-cabal/pull/20).

* Introduced new (automatic) cabal flag `Cabal-syntax` which should prevent
  Cabal's constraint solver from picking incompatible versions of `Cabal` and
  `Cabal-syntax` when using `cabal-install` to build jailbreak-cabal.
  See [#22](https://github.com/NixOS/jailbreak-cabal/pull/22).
