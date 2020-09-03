{-# LANGUAGE OverloadedStrings #-}

module MainIO where

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import System.IO
import System.Environment
import System.Exit
import System.Process
import System.Directory (copyFile, removeFile)
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
    writeFile backupPath stext
  bash <- except $ showErr $ parse "" stext
  let iniR = initReducer (checkExec ccvar) (replaceExec (packBashWord ccvar) (packBashWord xccvar)) bash
  when debug $ liftIO $ printf "All targets: %s\n" (intercalate "," (map show (getCandIxs iniR)))
  -- run sanity interestingness test
  mSanityError <- liftIO $ runSanityCheck env bash
  case mSanityError of
    Just err -> do
      liftIO $ putStrLn err
      runRecover 
      except $ Left "Sanity test failed"
    Nothing  -> do 
      target <- runReduction iniR env 1
      runRecover 
      return $ showByIxList target bash
  where
    ccvar      = ccVar               env
    xccvar     = xccVar              env
    fpath      = bashPath            env
    ipath      = interestingnessPath env
    recover    = recoverMakefile     env
    debug      = isDebug             env
    backupPath = fpath ++ ".orig"
    runRecover :: ExceptT String IO ()
    runRecover = liftIO $ when recover $ do
      copyFile backupPath fpath
      removeFile backupPath
      putStrLn "Recovered original script"


runSanityCheck :: Env -> List -> IO (Maybe String)
runSanityCheck env bash = do
  pass1 <- runItest bash "[Sanity Check]: Running the original interestingness test script..."
  if pass1 /= ExitSuccess then do
    let bash' = replaceAllExec ccvar xccvar bash
    pass2 <- runItest bash' "[Sanity Check]: Running the interestingness test script with CC replaced..."
    if pass2 == ExitSuccess then
      return Nothing
    else 
      return $ Just $ printf "2nd sanity test failed! Expecting exitcode = %s, but actual exitcode = %s" (show ExitSuccess) (show pass2)
  else do
     return $ Just $ printf "1st sanity test failed! Expecting exitcode to be ExitFailure, but actual exitcode = ExitSuccess"
  where
    ccvar          = ccVar               env
    xccvar         = xccVar              env
    fpath          = bashPath            env
    ipath          = interestingnessPath env
    debug          = isDebug             env
    runItest b msg = putStrLn msg >> testScript env b



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
  exit <- liftIO $ testScript env mf 
  except $ reduceFeedback reducer inss (exit == ExitSuccess)
  where 
    debug = isDebug env
    
testScript :: Env -> List -> IO ExitCode
testScript env bash = do
  writeFile fpath $ prettyText bash
  when debug $ do
    putStrLn "---------------"
    putStrLn $ prettyText bash
    putStrLn "---------------"
  result <- runExceptT $ do 
    expectScript "bash" [fpath] (== ExitSuccess)
    expectScript "bash" [ipath] (== ExitSuccess)
  case result of
    Left (cmd, code, out, err) -> do
      printf "FAIL :: %s\n" cmd
      when debug $ do
        putStrLn out
        putStrLn "-------------"
        putStrLn err
      return code
    Right (code, out, err) -> do 
      putStrLn "PASS"
      when debug $ putStrLn out
      return code
  where
    fpath = bashPath            env
    ipath = interestingnessPath env
    debug = isDebug             env

expectScript :: String -> [String] -> (ExitCode -> Bool) 
             -> ExceptT (String, ExitCode, String, String) IO (ExitCode, String, String)
expectScript bin args p = do
  liftIO $ putStrLn cmd
  (code, out, err) <- liftIO $ readProcessWithExitCode bin args ""
  if (p code) then
    return (code, out, err)
  else
    except $ Left (cmd, code, out, err)
  where
    cmd = unwords (bin : args)

logIO :: MonadIO m => String -> m ()
logIO = liftIO . putStrLn

showErr :: (Show e) => Either e a -> Either String a
showErr (Left err) = Left (show err)
showErr (Right a)  = Right a
