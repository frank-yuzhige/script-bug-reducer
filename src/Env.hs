{-# LANGUAGE OverloadedStrings #-}

module Env (
  Env (..),
  parseArgs
) where

import Control.Monad
import Text.Printf
import Utils.DList
import Data.Text (Text, pack, unpack)
import Data.String

import Control.Monad.Trans.Writer

type WString = DList Char

data Env = Env {
  ccVar               :: String,
  xccVar              :: String,
  bashPath            :: FilePath,
  interestingnessPath :: FilePath,
  recoverMakefile     :: Bool,
  isCache             :: Bool,
  isDebug             :: Bool
} deriving (Eq, Show)

initEnv :: Env
initEnv = Env {
  ccVar  = "gcc",
  xccVar = "clang",
  bashPath = "./build.sh",
  interestingnessPath = "./itest.sh",
  recoverMakefile = False,
  isCache = False,
  isDebug = False
}

parseArgs :: [String] -> (Env, String)
parseArgs args = (e, fromDList w)
  where (e, w) = runWriter $ foldM parseArg initEnv args

parseArg :: Env -> String -> Writer WString Env
parseArg env arg@('-': xs)
  | null qs   = case ps of
    "recover"    -> record 
      "Recover script after reduction" 
      env { recoverMakefile = True }
    "no-recover" -> record
      "Do not recover script after reduction"
      env { recoverMakefile = False } 
    "debug"      -> record 
      "DEBUG ON!"
      env { isDebug = True } 
    "cache"      -> record
      "Cache files where possible"
      env { isCache = True }
    _            -> warn $ printf "Invalid argument \"%s\"" arg
  | otherwise = case ps of
    "cvar"   -> record
      (printf "Set cc=%s" rs)
      env { ccVar = rs }
    "xcvar"  -> record
      (printf "Set xcc=%s" rs)
      env { xccVar = rs }
    "path"   -> record
      (printf "Set build script=%s" rs)
      env { bashPath = rs }
    "ipath"  -> record
      (printf "Set interestingness test script=%s" rs)
      env { interestingnessPath = rs }
    _        -> warn $ printf "Invalid argument \"%s\"" arg
  where
    (ps, qs) = break (== '=') xs
    rs       = tail qs
    warn w   = tell (toDList (w <> "\n")) >> return env 

parseArg env arg = tell (toDList (printf "Error: Invalid argument \"%s\"\n" arg)) >> return env

printEnv :: (PrintfType f) => Env -> f
printEnv env = printf (show env)

record :: Monad m => [Char] -> b -> WriterT (DList Char) m b
record w a = tell (toDList $ w <> "\n") >> return a
