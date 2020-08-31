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
  logIO $ printf "Making backup for the original bash script to %s ..." backupPath
  liftIO $ writeFile (fpath ++ ".orig") stext
  bash <- except $ showErr $ parse "" stext
  let iniR = initReducer (checkExec ccvar) (replaceExec (packBashWord ccvar) (packBashWord xccvar)) bash
  liftIO $ printf "All targets: %s\n" (intercalate "," (map show (getCandIxs iniR)))
  target <- runReduction iniR fpath ipath
  liftIO $ when recover $ do
    copyFile backupPath fpath
    putStrLn "Recovered original script"
  return $ showByIxList target bash
  where
    ccvar   = ccVar               env
    xccvar  = xccVar              env
    fpath   = bashPath            env
    ipath   = interestingnessPath env
    recover = recoverMakefile     env

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
  isPass <- liftIO $ testMakefile fpath ipath mf 
  except $ reduceFeedback reducer inss isPass
    
testMakefile :: FilePath -> FilePath -> List -> IO Bool
testMakefile fpath ipath bash = do
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