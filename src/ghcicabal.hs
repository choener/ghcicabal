
-- | Calls @ghci@ with all extensions of all subprojects, and also provides all
-- such paths to @ghci. This makes it possible to develop multiple packages,
-- that are interdependent, in parallel.

module Main where

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



data Opts = Opts
  { oRootDirs   ∷ [String]
    -- ^ recursive parsing from these roots
  , oIgnoreDirs ∷ [String]
    -- ^ do not offer these directories to ghci. These contain submodules whose contents are compiled by nix.
  , oMaxParseDepth  ∷ Int
    -- ^ maximal depth to recursively parse
  }

opts ∷ Parser Opts
opts = Opts
  <$> (  many (argument str (metavar "DIRS..."))
      )
  <*> option auto
      (  long "exclude"
      <>  short 'x'
      <> help "exclude these directories"
      <> showDefault
      <> value [ "./otherdeps" ]
      )
  <*> option auto
      (  long "depth"
      <> short 'd'
      <> help "directory depth to search for *.cabal files"
      <> showDefault
      <> value 1
      )

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
mkInfo f BuildInfo{..} = Info (S.fromList $ defaultExtensions ++ otherExtensions) (S.fromList . map (dropFileName f </>) $ "./" : hsSourceDirs)


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

dirCheck ∷ [String] → FindClause Bool
dirCheck = fmap not . foldM (\z i → contains i >>= return . (z||)) False

main = do
  Opts{..} ← execParser $ info (opts <**> helper) (fullDesc <> progDesc "run ghcicabal" <> header "ghcicabal: (c) Christian Hoener zu Siederdissen, 2019")
  let ds = if null oRootDirs then ["./", "./deps"] else oRootDirs
  fs ← concat <$> mapM (F.find (dirCheck oIgnoreDirs ||? depth <=? oMaxParseDepth) (extension ==? ".cabal")) ds
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

