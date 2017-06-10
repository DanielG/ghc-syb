module Main where

import GHC.Paths ( libdir )
import qualified GhcMake as Ghc
import qualified GHC as Ghc
import qualified HscTypes as Ghc
import           MonadUtils ( liftIO )
import System.Directory ( getCurrentDirectory )
import System.FilePath ( (</>) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( when )

import GHC.SYB.Utils

main :: IO ()
main = do
  pkg_dir <- getCurrentDirectory
  let ex1 = pkg_dir </> "test" </> "test-cases" </> "GithubIssue9.hs"
  Ghc.runGhc (Just libdir) $ do
    dflags0 <- Ghc.getSessionDynFlags
    let dflags = dflags0
          { Ghc.ghcLink = Ghc.NoLink
          , Ghc.hscTarget = Ghc.HscAsm
          }
    Ghc.setSessionDynFlags dflags
    env <- Ghc.getSession
    Ghc.handleSourceError printErrorAndExit $ do
      target <- Ghc.guessTarget ex1 Nothing
      Ghc.setTargets [target]
      ok <- Ghc.load Ghc.LoadAllTargets
      when (not (Ghc.succeeded ok)) $ die
      let mn = Ghc.mkModuleName "GithubIssue9"
      msum <- Ghc.getModSummary mn
      parsed <- Ghc.parseModule msum
      liftIO $ do
        putStrLn "===== Parsed Source =================================="
        putStrLn $ showData Parser 1 (Ghc.parsedSource parsed)
      typechecked <- Ghc.typecheckModule parsed
      liftIO $ do
        putStrLn "===== Renamed Source ================================="
        putStrLn $ showData Renamer 1 (Ghc.renamedSource typechecked)
        putStrLn "===== Type-checked Source ============================"
        putStrLn $ showData TypeChecker 1 (Ghc.typecheckedSource typechecked)
      return ()

printErrorAndExit :: Ghc.SourceError -> Ghc.Ghc ()
printErrorAndExit err = Ghc.printException err >> die

die :: Ghc.Ghc ()
die = liftIO $ exitWith (ExitFailure 1)
