{-# LANGUAGE OverloadedStrings #-}

module MainIO where

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import System.IO
import System.Environment
import System.Exit
import System.Process
import System.Directory(copyFile)
import Data.List
import Language.Bash.Syntax
import Language.Bash.Parse
import Language.Bash.Pretty
import Reduce.Bash
import Reduce.Reducer
import Env
import Text.Printf (printf)

import Control.Monad.Trans.Except
import Control.Monad.Trans
import Control.Monad.Except

mainIO :: IO ()
mainIO = do
  target <- runExceptT $ do 
    menv <- liftIO $ parseArgs <$> getArgs
    env  <- except menv
    liftIO $ printf "%s\n" (show env)
    reduceToCmd env
  case target of
    Left err -> printf "Error: %s\n" err
    Right t  -> printf "Found error command: %s\n" (show t)

reduceToCmd :: Env -> ExceptT String IO String
reduceToCmd env = do
  stext <- liftIO $ readFile fpath
  let backupPath = fpath ++ ".orig"
  liftIO $ do 
    printf "Making backup for the original bash script to %s ...\n" backupPath
    writeFile (fpath ++ ".orig") stext
  bash <- except $ showErr $ parse "" stext
  let iniR = initReducer (checkExec ccvar) (replaceExec (packBashWord ccvar) (packBashWord xccvar)) bash
  liftIO $ printf "All targets: %s\n" (intercalate "," (map show (getCandIxs iniR)))
  -- run sanity interestingness test
  mSanityError <- liftIO $ runSanityCheck env bash
  case mSanityError of
    Just err -> do
      liftIO $ putStrLn err
      runRecover backupPath
      except $ Left "Sanity test failed"
    Nothing  -> do 
      target <- runReduction iniR fpath ipath
      runRecover backupPath
      return $ showByIxList target bash
  where
    ccvar      = ccVar               env
    xccvar     = xccVar              env
    fpath      = bashPath            env
    ipath      = interestingnessPath env
    recover    = recoverMakefile     env
    runRecover :: FilePath -> ExceptT String IO ()
    runRecover backupPath = liftIO $ when recover $ do
      copyFile backupPath fpath
      putStrLn "Recovered original script"


runSanityCheck :: Env -> List -> IO (Maybe String)
runSanityCheck env bash = do
  (code, out, err) <- runItest "[Sanity Check]: Running the original interestingness test script..."
  if (code /= ExitSuccess) then do
    let bash' = replaceAllExec ccvar xccvar bash
    writeFile fpath $ prettyText bash' 
    (code', out', err') <- runItest "[Sanity Check]: Running the interestingness test script with CC replaced..."
    if (code' == ExitSuccess) then
      return Nothing
    else 
      return $ Just $ printf "2nd sanity test failed! Expecting exitcode = %s, but actual exitcode = %s" (show ExitSuccess) (show code')
  else do
     return $ Just $ printf "1st sanity test failed! Expecting exitcode to be ExitFailure, but actual exitcode = ExitSuccess"
  where
    ccvar          = ccVar               env
    xccvar         = xccVar              env
    fpath          = bashPath            env
    ipath          = interestingnessPath env
    runItest msg   = putStrLn msg >> liftIO (readProcessWithExitCode "sh" [ipath] "")


runReduction :: Reducer -> FilePath -> FilePath -> ExceptT String IO CmdIx
runReduction r fpath ipath = do
  r' <- reduceIteration r fpath ipath
  maybe (continue r') found (terminateReduce r') 
  where    
    continue :: Reducer -> ExceptT String IO CmdIx
    continue r = do
      liftIO $ putStrLn $ printf "continue..."
      runReduction r fpath ipath
    found :: CmdIx -> ExceptT String IO CmdIx
    found t = logIO "found!" >> return t

reduceIteration :: Reducer -> FilePath -> FilePath -> ExceptT String IO Reducer
reduceIteration reducer fpath ipath = do
  (mf, inss) <- except $ genNewBash reducer
  let count = length inss
  liftIO $ printf "Inspecting %d target%s: %s\n" 
    count 
    (if count > 1 then "s" else "" :: String) 
    (unwords $ map show inss)
  isPass <- liftIO $ testScript fpath ipath mf 
  except $ reduceFeedback reducer inss isPass
    
testScript :: FilePath -> FilePath -> List -> IO Bool
testScript fpath ipath bash = do
  writeFile fpath $ prettyText bash
  putStrLn "---------------"
  putStrLn $ prettyText bash
  putStrLn "---------------"
  (code, out, err) <- readProcessWithExitCode "sh" [ipath] ""
  if code == ExitSuccess then do
    putStrLn ">> PASS"
    putStrLn out
    return True
  else do
    putStrLn ">> FAIL"
    putStrLn out
    putStrLn "-------------"
    putStrLn err
    return False

logIO :: MonadIO m => String -> m ()
logIO = liftIO . putStrLn

showErr :: (Show e) => Either e a -> Either String a
showErr (Left err) = Left (show err)
showErr (Right a)  = Right a