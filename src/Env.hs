{-# LANGUAGE OverloadedStrings #-}

module Env (
  Env (..),
  parseArgs
) where

import Control.Monad
import Text.Printf
import Data.Text (Text, pack, unpack)

data Env = Env {
  ccVar               :: Text,
  xccVar              :: Text,
  xccCompiler         :: Text,
  makefilePath        :: FilePath,
  makeTarget          :: [String],          
  interestingnessPath :: FilePath,

  recoverMakefile     :: Bool
} deriving (Eq, Show)

parseArgs :: [String] -> Either String Env
parseArgs = foldM parseArg initEnv 

parseArg :: Env -> String -> Either String Env
parseArg env arg@('-': xs)
  | null qs   = case ps of
    "recover-make"    -> Right env { recoverMakefile = True }
    "no-recover-make" -> Right env { recoverMakefile = False } 
    _                 -> err
  | otherwise = case ps of
    "cvar"   -> Right env { ccVar               = pack rs }
    "xcvar"  -> Right env { xccVar              = pack rs }
    "xcc"    -> Right env { xccCompiler         = pack rs }
    "target" -> Right env { makeTarget          = [rs] }
    "ipath"  -> Right env { interestingnessPath = rs }
    _        -> err
  where
    (ps, qs) = break (== '=') xs
    rs       = tail qs
    err      = Left $ printf "Error: Invalid argument \"%s\"" arg

parseArg _ arg = Left $ printf "Error: Invalid argument \"%s\"" arg

initEnv :: Env
initEnv = Env {
  ccVar  = "CC",
  xccVar = "XCC",
  xccCompiler = "clang",
  makefilePath = "Makefile",
  makeTarget = [],
  interestingnessPath = "./itest.sh",
  recoverMakefile = False
}