module Main where

import MainIO
import Language.Bash.Parse
import Language.Bash.Pretty
import Reduce.Bash

main :: IO ()
main = mainIO
  -- content <- readFile "dummy.sh"
  -- let Right x = parse "123" content
  -- print x
  -- putStrLn $ prettyText x
  -- putStrLn "----------------"
  -- let y =  replaceExecAt [1, 0, 0, 1, 0, 0, 0] (packBashWord "echo") (packBashWord "printf") x
  -- putStrLn $ prettyText y
  -- print $ getIxsList (checkExec "echo") y
