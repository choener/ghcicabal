
-- | Calls @ghci@ with all extensions of all subprojects, and also provides all
-- such paths to @ghci. This makes it possible to develop multiple packages,
-- that are interdependent, in parallel.

module Main where

import Data.List (intersperse)
import Distribution.PackageDescription.Parsec
import Distribution.Pretty
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.Executable
import Distribution.Types.ForeignLib
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Distribution.Verbosity
import GHC.Generics(Generic)
import Language.Haskell.Extension
import qualified Data.Set as S
import qualified System.Process as P
import System.Environment
import System.FilePath
import System.FilePath.Find as F
import Text.Printf



-- | Extract the necessary information from all packages. Two @Info@s can be
-- combined using @<>@, and the final @Info@ yields the correct environment.

data Info = Info
  { iExts ∷ S.Set Extension
  , iPaths  ∷ S.Set FilePath
  }
  deriving (Eq,Ord,Show,Generic)

instance Semigroup Info where
  Info ae ap <> Info be bp = Info (ae<>be) (ap<>bp)

instance Monoid Info where
  mempty = Info S.empty S.empty

-- | Creates an 'Info' from a 'BuildInfo'.

mkInfo ∷ FilePath → BuildInfo → Info
mkInfo f BuildInfo{..} = Info (S.fromList $ defaultExtensions ++ otherExtensions) (S.fromList $ map (dropFileName f </>) hsSourceDirs)


-- | Given a filepath and a package description, return the 'Info'.

extensions ∷ FilePath → GenericPackageDescription → Info
extensions f p = cl <> mconcat csls <> mconcat cfls <> mconcat ces
    -- TODO test suites and benchmarks
  where
    lib = libBuildInfo . condTreeData
    cl   = maybe mempty (mkInfo f . lib) (condLibrary p)
    csls = [ mkInfo f $ lib t | (_,t) ← condSubLibraries p ]
    cfls = [ mkInfo f . foreignLibBuildInfo $ condTreeData t | (_,t) ← condForeignLibs p ]
    ces  = [ mkInfo f . buildInfo $ condTreeData t | (_,t) ← condExecutables p ]

-- | Prepare 'Extension' as @ghci@ argument.

extString ∷ Extension → String
extString = ("-X" ++) . prettyShow

main = do
  as ← getArgs
  let ds = if null as then ["./"] else as
  fs ← concat <$> mapM (F.find always (extension ==? ".cabal")) ds
  ps ← mapM (readGenericPackageDescription silent) fs
  let z = mconcat $ zipWith extensions fs ps
  -- cobble together the command line
  let cmdline = printf "ghci %s -i%s"
        -- all extensions
        (concat . intersperse " " . map extString . S.toList $ iExts  z)
        -- all directories, extracted from cabal files
        (concat . intersperse ":" . S.toList $ iPaths z)
  putStrLn cmdline
  P.callCommand cmdline

