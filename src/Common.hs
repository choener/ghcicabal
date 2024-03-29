
module Common where

import Control.Monad (foldM,liftM2)
import Data.List (intersperse,isSuffixOf)
import Distribution.PackageDescription.Parsec
import Distribution.Pretty
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.Executable
import Distribution.Types.ForeignLib
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Distribution.Types.TestSuite
import Distribution.Verbosity
import GHC.Generics(Generic)
import Language.Haskell.Extension
import Options.Applicative
import qualified Data.Set as S
import qualified System.Process as P
import System.Environment
import System.FilePath
import System.FilePath.Find as F
import Text.Printf
import Debug.Trace



data Opts = Opts
  { oRootDirs   ∷ [String]
    -- ^ recursive parsing from these roots
  , oIgnoreDirs ∷ [String]
    -- ^ do not offer these directories to ghci. These contain submodules whose contents are compiled by nix.
  , oMaxParseDepth  ∷ Int
    -- ^ maximal depth to recursively parse
  , oWorkFile :: FilePath
  }

pWorkFile :: Parser FilePath
pWorkFile = argument str (metavar "FILE")

opts ∷ Parser FilePath -> Parser Opts
opts pfp = Opts
{-
  <$> (  many (argument str (metavar "DIRS..."))
      ) -}
  <$> option auto
      (  long "root"
      <>  short 'r'
      <> help "root dirs"
      <> showDefault
      <> value [] --  "./otherdeps" ]
      )
  <*> option auto
      (  long "exclude"
      <>  short 'x'
      <> help "exclude these directories"
      <> showDefault
      <> value [ "otherdeps" ]
      )
  <*> option auto
      (  long "depth"
      <> short 'd'
      <> help "directory depth to search for *.cabal files"
      <> showDefault
      <> value 1
      )
  <*> pfp

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

mkInfo ∷ FilePath -> BuildInfo -> Info
mkInfo f BuildInfo{..} = Info (S.fromList $ defaultExtensions ++ otherExtensions) (S.fromList . map (dropFileName f </>) $ "./" : hsSourceDirs)


-- | Given a filepath and a package description, return the 'Info'.

extensions ∷ FilePath -> GenericPackageDescription -> Info
extensions f p = cl <> mconcat csls <> mconcat cfls <> mconcat ces <> mconcat cts
    -- TODO test suites and benchmarks
  where
    lib = libBuildInfo . condTreeData
    cl   = maybe mempty (mkInfo f . lib) (condLibrary p)
    csls = [ mkInfo f $ lib t | (_,t) <- condSubLibraries p ]
    cfls = [ mkInfo f . foreignLibBuildInfo $ condTreeData t | (_,t) <- condForeignLibs p ]
    ces  = [ mkInfo f . buildInfo $ condTreeData t | (_,t) <- condExecutables p ]
    -- test suites
    cts  = [ mkInfo f . testBuildInfo $ condTreeData t | (_,t) <- condTestSuites p ]

-- | Prepare 'Extension' as @ghci@ argument.

extString ∷ Extension -> String
extString = ("-X" ++) . prettyShow

-- | Each @i@ is a directory to ignore. So @i \in ["./ignore"]@ should have us ignore directories
-- under ignore.

dirCheck ∷ [String] -> FindClause Bool
dirCheck = fmap not . foldM (\z i -> (canonicalPath ~~? i) >>= return . (z||)) False

runMain :: String -> Parser FilePath -> IO ()
runMain exe pfp = do
  Opts{..} <- execParser $ info (opts pfp <**> helper) (fullDesc <> progDesc "run ghcicabal" <> header "ghcicabal: (c) Christian Hoener zu Siederdissen, 2019-2020")
  let ds = if null oRootDirs then ["./", "deps"] else oRootDirs
  -- cabal related
  cs <- concat <$> mapM (F.find (dirCheck oIgnoreDirs &&? depth <=? oMaxParseDepth) (extension ==? ".cabal")) ds
  ps <- mapM (readGenericPackageDescription silent) $ cs
  let z = mconcat $ zipWith extensions cs ps
  -- hpack related
  ys <- concat <$> mapM (F.find (dirCheck oIgnoreDirs &&? depth <=? oMaxParseDepth) (fileName ==? "package.yaml")) ds
  -- TODO parse package.yaml files and create extensions
  -- cobble together the command line
  let cmdline = printf "%s %s -i%s%s" exe
        -- all extensions
        (concat . intersperse " " . S.toList . S.map extString $ iExts  z)
        -- all directories, extracted from cabal files
        (concat . intersperse ":" . S.toList $ iPaths z)
        (if null oWorkFile then "" else ' ': oWorkFile)
  putStrLn cmdline
  P.callCommand cmdline

