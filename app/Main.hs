module Main where

import System.Plex
import System.Environment

main :: IO ()
main = do
  [theCmd, argStr] <- getArgs
  putStrLn $ "started. cmd = '" ++ show theCmd ++ ", argStr = '" ++ show argStr ++ "'"
  let args = read argStr
  output <- cmdTimeout theCmd args (2 * 10 ^ 6)
  putStrLn $ "output was: '" ++ show output ++ "'"
  putStrLn "done. say something:"
  ln <- getLine
  putStrLn $ "line was: " ++ show ln
  
