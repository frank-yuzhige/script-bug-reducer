{-# LANGUAGE OverloadedStrings #-}

module Env (
  Env (..),
  parseArgs
) where

import Control.Monad
import Text.Printf
import Data.Text (Text, pack, unpack)

data Env = Env {
  ccVar               :: String,
  xccVar              :: String,
  bashPath            :: FilePath,
  interestingnessPath :: FilePath,
  recoverMakefile     :: Bool
} deriving (Eq, Show)

parseArgs :: [String] -> Either String Env
parseArgs = foldM parseArg initEnv 

parseArg :: Env -> String -> Either String Env
parseArg env arg@('-': xs)
  | null qs   = case ps of
    "recover"    -> Right env { recoverMakefile = True }
    "no-recover" -> Right env { recoverMakefile = False } 
    _                 -> err
  | otherwise = case ps of
    "cvar"   -> Right env { ccVar               = rs }
    "xcvar"  -> Right env { xccVar              = rs }
    "path"   -> Right env { bashPath            = rs }
    "ipath"  -> Right env { interestingnessPath = rs }
    _        -> err
  where
    (ps, qs) = break (== '=') xs
    rs       = tail qs
    err      = Left $ printf "Error: Invalid argument \"%s\"" arg

parseArg _ arg = Left $ printf "Error: Invalid argument \"%s\"" arg

initEnv :: Env
initEnv = Env {
  ccVar  = "gcc",
  xccVar = "clang",
  bashPath = "./build.sh",
  interestingnessPath = "./itest.sh",
  recoverMakefile = False
}

printEnv :: (PrintfType f) => Env -> f
printEnv env = printf (show env)