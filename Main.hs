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

-- We don't relax version restrictions inside conditional statements.
-- See https://github.com/peti/jailbreak-cabal/commit/99eac40deb481b185fd93fd307625369ff5e1ec0
stripVersionRestrictions :: GenericPackageDescription -> GenericPackageDescription
stripVersionRestrictions pkg = pkg { condLibrary = fmap relaxLibraryTree (condLibrary pkg)
                                   , condExecutables = map (fmap relaxExeTree) (condExecutables pkg)
                                   , condTestSuites = map (fmap relaxTestTree) (condTestSuites pkg)
                                   }
  where
    relaxTreeConstraints :: CondTree v [Dependency] a -> CondTree v [Dependency] a
    relaxTreeConstraints ct = ct { condTreeConstraints = map relax (condTreeConstraints ct) }

    relaxLibraryTree :: CondTree v [Dependency] Library -> CondTree v [Dependency] Library
    relaxLibraryTree ct = relaxTreeConstraints $ ct { condTreeData = relaxLibrary (condTreeData ct) }

    relaxExeTree :: CondTree v [Dependency] Executable -> CondTree v [Dependency] Executable
    relaxExeTree ct = relaxTreeConstraints $ ct { condTreeData = relaxExe (condTreeData ct) }

    relaxTestTree :: CondTree v [Dependency] TestSuite -> CondTree v [Dependency] TestSuite
    relaxTestTree ct = relaxTreeConstraints $ ct { condTreeData = relaxTest (condTreeData ct) }

    relaxLibrary :: Library -> Library
    relaxLibrary l = l { libBuildInfo = relaxBuildInfo (libBuildInfo l) }

    relaxExe :: Executable -> Executable
    relaxExe e = e { buildInfo = relaxBuildInfo (buildInfo e) }

    relaxTest :: TestSuite -> TestSuite
    relaxTest t = t { testBuildInfo = relaxBuildInfo (testBuildInfo t) }

    relaxBuildInfo :: BuildInfo -> BuildInfo
    relaxBuildInfo bi = bi { buildTools = map relax (buildTools bi)
                           , targetBuildDepends = map relax (targetBuildDepends bi)
                           }

    relax :: Dependency -> Dependency
    relax (Dependency d _) = Dependency d anyVersion
