module Main ( main ) where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint
import Distribution.Version
import Distribution.Verbosity
import System.Environment

main :: IO ()
main = getArgs >>= mapM_ (\cabalFile -> readPackageDescription silent cabalFile >>= writeGenericPackageDescription cabalFile . stripVersionRestrictions)

stripVersionRestrictions :: GenericPackageDescription -> GenericPackageDescription
stripVersionRestrictions pkg = pkg { condLibrary = fmap f2 (condLibrary pkg)
                                   , condExecutables = map f1 (condExecutables pkg)
                                   , condTestSuites = map f1 (condTestSuites pkg)
                                   }
  where
    f1 (string,condTree) = (string, f2 condTree)
    f2 ct = ct { condTreeConstraints = map f3 (condTreeConstraints ct) }
    f3 (Dependency d _) = Dependency d anyVersion
