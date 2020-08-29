module Main where

import MainIO
import Language.Bash.Parse
import Language.Bash.Pretty
import Reduce.Bash

main :: IO ()
main = do
  content <- readFile "dummy.sh"
  let Right x = parse "123" content
  print x
  print $ prettyText x
  putStrLn "----------------"
  let y =  replaceExecAt (toCmdIx [0, 1, 0]) (packBashWord "echo") (packBashWord "printf") x
  print $ prettyText y
