
module Main where

import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.ForeignLib
import Distribution.Types.Executable
import Language.Haskell.Extension
import qualified Data.Set as S
import qualified System.Process as P
import Text.Printf
import Distribution.Pretty
import Data.List (intersperse)
import System.Environment
import System.FilePath.Find as F
import GHC.Generics(Generic)
import System.FilePath



data Info = Info
  { iExts ∷ S.Set Extension
  , iPaths  ∷ S.Set FilePath
  }
  deriving (Eq,Ord,Show,Generic)

instance Semigroup Info where
  Info ae ap <> Info be bp = Info (ae<>be) (ap<>bp)

instance Monoid Info where
  mempty = Info S.empty S.empty

mkInfo ∷ FilePath → BuildInfo → Info
mkInfo f BuildInfo{..} = Info (S.fromList $ defaultExtensions ++ otherExtensions) (S.fromList $ map (dropFileName f </>) hsSourceDirs)


-- | returns all extensions used in the cabal file

extensions ∷ FilePath → GenericPackageDescription → Info
extensions f p = cl <> mconcat csls <> mconcat cfls <> mconcat ces
--    ++ cfls
--    ++ ces
    -- TODO test suites and benchmarks
  where
    lib = libBuildInfo . condTreeData
    cl   = maybe mempty (mkInfo f . lib) (condLibrary p)
    csls = [ mkInfo f $ lib t | (_,t) ← condSubLibraries p ]
    cfls = [ mkInfo f . foreignLibBuildInfo $ condTreeData t | (_,t) ← condForeignLibs p ]
    ces  = [ mkInfo f . buildInfo $ condTreeData t | (_,t) ← condExecutables p ]

extString ∷ Extension → String
extString = ("-X" ++) . prettyShow

withDir ∷ String → String
withDir = ("-i" ++)

main = do
  ghcLibDir ← maybe "" id <$> lookupEnv "NIX_GHC_LIBDIR"
  as ← getArgs
  let ds = if null as then ["./"] else as
  fs ← concat <$> mapM (F.find always (extension ==? ".cabal")) ds
  mapM_ print fs
  ps ← mapM (readGenericPackageDescription silent) fs
  let z = mconcat $ zipWith extensions fs ps
  print z
  -- cobble together the command line
  let cmdline = printf "ghci %s %s"
        -- all extensions
        (concat . intersperse " " . map extString . S.toList $ iExts  z)
        -- all directories, extracted from cabal files
        (concat . intersperse " " . map withDir   . S.toList $ iPaths z)
  putStrLn cmdline
  P.callCommand cmdline

