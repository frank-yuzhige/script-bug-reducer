{-# LANGUAGE OverloadedStrings #-}

module MainIO where

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import System.IO
import System.Environment
import System.Exit
import System.Process
import System.Directory(copyFile)
import Data.Makefile
import Data.Makefile.Parse
import Data.Makefile.Render
import Make.Make
import Make.Reducer
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
    reduceToTarget env
  case target of
    Left err -> printf "Error: %s\n" err
    Right t  -> printf "Found error target: %s\n" (show t)

reduceToTarget :: Env -> ExceptT String IO Target
reduceToTarget env = do
  mtext <- liftIO $ T.readFile mpath
  let backupPath = mpath ++ ".orig"
  logIO $ printf "Making backup for the original Makefile to %s ..." backupPath
  liftIO $ T.writeFile (mpath ++ ".orig") mtext
  let make = parseMakefileContents mtext
  iniR   <- except $ initReducer ccvar xccvar xcc <$> make  
  target <- runReduction iniR mpath iPath
  liftIO $ when recover $ do
    copyFile backupPath mpath
    putStrLn "Recovered original Makefile"
  return target
  where
    ccvar   = ccVar               env
    xccvar  = xccVar              env
    xcc     = xccCompiler         env
    mpath   = makefilePath        env
    targets = makeTarget          env
    iPath   = interestingnessPath env
    recover = recoverMakefile     env

runReduction :: Reducer -> FilePath -> FilePath -> ExceptT String IO Target
runReduction r mPath iPath = do
  r' <- reduceIteration r mPath
  maybe (continue r') found (terminateReduce r') 
  where    
    continue :: Reducer -> ExceptT String IO Target
    continue r = do
      liftIO $ putStrLn $ printf "continue..."
      runReduction r mPath iPath
    found :: Target -> ExceptT String IO Target
    found t = logIO "found!" >> return t

reduceIteration :: Reducer -> FilePath -> ExceptT String IO Reducer
reduceIteration reducer mPath = do
  (mf, inss) <- except $ genNewMakefile reducer
  let count = length inss
  liftIO $ printf "Inspecting %d target%s: %s\n" 
    count 
    (if count > 1 then "s" else "" :: String) 
    (unwords $ [ T.unpack t | Target t <- inss ])
  isPass <- liftIO $ testMakefile "Makefile" mf 
  except $ reduceFeedback reducer inss isPass
    
testMakefile :: FilePath -> Makefile -> IO Bool
testMakefile mPath make = do
  writeMakefile mPath make
  (code, out, err) <- readProcessWithExitCode "make" [] ""
  if code == ExitSuccess then
    return True
  else
    return False

(>>?=) :: Either String t -> (t -> IO ()) -> IO ()
Left err >>?= _ = putStrLn err
Right a  >>?= f = f a

logIO :: MonadIO m => String -> m ()
logIO = liftIO . putStrLn
