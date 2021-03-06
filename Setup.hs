import Control.Monad
import Control.Applicative

import Data.List
import Data.Maybe

import Data.Accessor

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.ModuleName hiding (main)
import Distribution.PackageDescription
import Distribution.Verbosity
import Distribution.Simple.Utils
import System.Process
import System.Exit
import System.FilePath
import System.Directory

protoDir :: FilePath
protoDir = "proto"

destDir :: FilePath
destDir = "dist/build/autogen"

-- accessors to make subsubrecord access a bit less mess...
accLocalPkgDescr :: Accessor LocalBuildInfo PackageDescription
accLocalPkgDescr = accessor localPkgDescr (\a r -> r {localPkgDescr = a})

-- go through Maybe hoping that it is not empty
accLibrary :: Accessor PackageDescription Library
accLibrary = accessor (fromJust . library) (\a r -> r {library = Just a})

accBuildInfo :: Accessor Library BuildInfo
accBuildInfo = accessor libBuildInfo (\a r -> r {libBuildInfo = a})

accOtherModules :: Accessor BuildInfo [ModuleName]
accOtherModules = accessor otherModules (\a r -> r {otherModules = a})

accExposedModules :: Accessor Library [ModuleName]
accExposedModules = accessor exposedModules (\a r -> r {exposedModules = a})

-- compile .proto file and return a list of generated modules parsed from hprotoc stdout
protoCompile :: Verbosity -> FilePath -> IO [String]
protoCompile v src = do
  notice v $ "Compiling proto definition: " ++ src
  let args = ["--proto_path=" ++ protoDir,"--haskell_out=" ++ destDir, protoDir </> src]
  (ec,out,_) <- readProcessWithExitCode "hprotoc" args ""
  when (ec /= ExitSuccess) $ die $ "hprotoc failed: " ++ show ec
  return $ map (replace '/' '.' . takeWhile (/= '.')) . 
    catMaybes . map (stripPrefix $ destDir ++ "/") . lines $ out
  where
    replace x y = map (\c -> if c == x then y else c)

-- call main configuration routine, then generate haskell sources and push their module names 
-- to the list of other modules of the library
myConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
myConfHook x cf = do
  lbi <- (confHook simpleUserHooks) x cf
  let verb = fromFlag $ configVerbosity $ cf
  protoFiles <- filter (".proto" `isSuffixOf`) <$> getDirectoryContents protoDir
  modList <- nub . map fromString . concat <$> mapM (protoCompile verb) protoFiles
  --return $ (accLocalPkgDescr ^: accLibrary ^: accBuildInfo ^: accOtherModules ^: (++modList)) lbi
  return $ (accLocalPkgDescr ^: accLibrary ^: accExposedModules ^: (++modList)) lbi

main :: IO ()
main = let hooks = simpleUserHooks 
       in defaultMainWithHooks hooks {confHook = myConfHook}
