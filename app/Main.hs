module Main where

import Evaluator

main :: IO ()
main = loop
  where
    loop :: IO ()
    loop = do
      putStrLn "Enter Expression (:q to exit): "
      input <- getLine
      if input == ":q"
        then putStrLn "Goodbye!"
        else do
          case readExpr input >>= evalNode of
            Just val -> putStrLn $ "Result: " ++ show val
            Nothing -> putStrLn "Error: Invalid expression"
          loop