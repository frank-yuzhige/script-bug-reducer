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
    (env, log) <- liftIO $ parseArgs <$> getArgs
    liftIO $ putStrLn log
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
  when debug $ liftIO $ printf "All targets: %s\n" (intercalate "," (map show (getCandIxs iniR)))
  -- run sanity interestingness test
  mSanityError <- liftIO $ runSanityCheck env bash
  case mSanityError of
    Just err -> do
      liftIO $ putStrLn err
      runRecover backupPath
      except $ Left "Sanity test failed"
    Nothing  -> do 
      target <- runReduction iniR env 1
      runRecover backupPath
      return $ showByIxList target bash
  where
    ccvar      = ccVar               env
    xccvar     = xccVar              env
    fpath      = bashPath            env
    ipath      = interestingnessPath env
    recover    = recoverMakefile     env
    debug      = isDebug             env
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
    when debug $ putStrLn $ prettyText bash'
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
    debug          = isDebug             env
    runItest msg   = putStrLn msg >> liftIO (readProcessWithExitCode "sh" [ipath] "")


runReduction :: Reducer -> Env -> Int -> ExceptT String IO CmdIx
runReduction r env count = do
  liftIO $ printf "Running iteration no.[%d]...\n" count
  r' <- reduceIteration r env
  maybe (continue r') found (terminateReduce r') 
  where    
    continue :: Reducer -> ExceptT String IO CmdIx
    continue r = do
      liftIO $ putStrLn $ printf "continue..."
      runReduction r env (count + 1)
    found :: CmdIx -> ExceptT String IO CmdIx
    found t = logIO "found!" >> return t

reduceIteration :: Reducer -> Env -> ExceptT String IO Reducer
reduceIteration reducer env = do
  when debug $ liftIO $ printf "  Generating new script\n"
  (mf, inss) <- except $ genNewBash reducer
  let count = length inss
  when debug $ liftIO $ printf "  Inspecting %d target%s: %s\n" 
    count 
    (if count > 1 then "s" else "" :: String) 
    (unwords $ map show inss)
  when debug $ liftIO $ printf "  Running interestingness test: \n"
  isPass <- liftIO $ testScript env mf 
  except $ reduceFeedback reducer inss isPass
  where 
    debug = isDebug env
    
testScript :: Env -> List -> IO Bool
testScript env bash = do
  writeFile fpath $ prettyText bash
  when debug $ do
    putStrLn "---------------"
    putStrLn $ prettyText bash
    putStrLn "---------------"
  (code, out, err) <- readProcessWithExitCode "sh" [ipath] ""
  if code == ExitSuccess then do
    putStrLn "PASS"
    when debug $ putStrLn out
    return True
  else do
    putStrLn "FAIL"
    when debug $ do
      putStrLn out
      putStrLn "-------------"
      putStrLn err
    return False
  where
    fpath = bashPath            env
    ipath = interestingnessPath env
    debug = isDebug             env

logIO :: MonadIO m => String -> m ()
logIO = liftIO . putStrLn

showErr :: (Show e) => Either e a -> Either String a
showErr (Left err) = Left (show err)
showErr (Right a)  = Right a
