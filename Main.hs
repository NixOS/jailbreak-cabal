{-# LANGUAGE CPP #-}
module Main ( main ) where

import Distribution.Package
#if MIN_VERSION_Cabal_syntax(3,7,0)
import Distribution.Simple.PackageDescription
#endif
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.PrettyPrint
import Distribution.Types.ExeDependency
import Distribution.Types.LegacyExeDependency
import Distribution.Verbosity
import Distribution.Version
import System.Environment

main :: IO ()
main = getArgs >>= mapM_ (\cabalFile -> readGenericPackageDescription silent cabalFile >>= writeGenericPackageDescription cabalFile . stripVersionRestrictions)

-- We don't relax version restrictions inside conditional statements.
-- See https://github.com/peti/jailbreak-cabal/commit/99eac40deb481b185fd93fd307625369ff5e1ec0
stripVersionRestrictions :: GenericPackageDescription -> GenericPackageDescription
stripVersionRestrictions pkg = pkg { condLibrary = fmap relaxLibraryTree (condLibrary pkg)
                                   , condSubLibraries = map (\(n, l) -> (n, relaxLibraryTree l)) (condSubLibraries pkg)
                                   , condExecutables = map (fmap relaxExeTree) (condExecutables pkg)
                                   , condTestSuites = map (fmap relaxTestTree) (condTestSuites pkg)
                                   }

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
                       , buildToolDepends = map relax (buildToolDepends bi)
                       , targetBuildDepends = map relax (targetBuildDepends bi)
                       }

class DependencyType a where
  relax :: a -> a

instance DependencyType Dependency where
  relax (Dependency d _ deps) = Dependency d anyVersion deps

instance DependencyType ExeDependency where
  relax (ExeDependency d u _) = ExeDependency d u anyVersion

instance DependencyType LegacyExeDependency where
  relax (LegacyExeDependency d _) = LegacyExeDependency d anyVersion
