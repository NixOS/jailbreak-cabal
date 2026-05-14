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
#if MIN_VERSION_Cabal_syntax(3,14,0)
import Distribution.Utils.Path
#endif
import Distribution.Verbosity
import Distribution.Version
import System.Environment

main :: IO ()
main = getArgs >>= mapM_ jailbreakFile

jailbreakFile :: FilePath -> IO ()
jailbreakFile cabalFile = do
  readGenericPackageDescription
#if MIN_VERSION_Cabal(3,17,0)
    (mkVerbosity defaultVerbosityHandles silent)
#else
    silent
#endif
#if MIN_VERSION_Cabal(3,14,0)
    -- Do not give a CWD directory, so the working dir of the process is used (like before).
    -- Note: Passing a CWD directory is inadvisable since writeGenericPackageDescription
    -- does not support it, so we'd get differing behavior.
    Nothing (makeSymbolicPath cabalFile)
#else
    cabalFile
#endif
  >>= writeGenericPackageDescription cabalFile . stripVersionRestrictions

-- We don't relax version restrictions inside conditional statements.
-- See https://github.com/peti/jailbreak-cabal/commit/99eac40deb481b185fd93fd307625369ff5e1ec0
stripVersionRestrictions :: GenericPackageDescription -> GenericPackageDescription
stripVersionRestrictions pkg = pkg { condLibrary = fmap relaxLibraryTree (condLibrary pkg)
                                   , condSubLibraries = map (\(n, l) -> (n, relaxLibraryTree l)) (condSubLibraries pkg)
                                   , condExecutables = map (fmap relaxExeTree) (condExecutables pkg)
                                   , condTestSuites = map (fmap relaxTestTree) (condTestSuites pkg)
                                   }

type CompatCondTree v comp =
#if MIN_VERSION_Cabal(3,17,0)
  CondTree v comp
#else
  CondTree v [Dependency] comp
#endif

relaxTreeConstraints :: CompatCondTree v a -> CompatCondTree v a
#if MIN_VERSION_Cabal(3,17,0)
relaxTreeConstraints = id
#else
relaxTreeConstraints ct = ct { condTreeConstraints = map relax (condTreeConstraints ct) }
#endif

relaxLibraryTree :: CompatCondTree v Library -> CompatCondTree v Library
relaxLibraryTree ct = relaxTreeConstraints $ ct { condTreeData = relaxLibrary (condTreeData ct) }

relaxExeTree :: CompatCondTree v Executable -> CompatCondTree v Executable
relaxExeTree ct = relaxTreeConstraints $ ct { condTreeData = relaxExe (condTreeData ct) }

relaxTestTree :: CompatCondTree v TestSuite -> CompatCondTree v TestSuite
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
