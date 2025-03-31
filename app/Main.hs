module Main where

import Evaluator

main :: IO ()
main = loop
    where
        loop :: IO ()
        loop = do
            putStr "Enter Expression (:q to exit): "
            input <- getLine
            if input == ":q"
               then putStrLn "Goodbye!"
               else do
                   let result = evalExpr $ parseExpr $ tokenize $ input
                   putStrLn $ "Result: " ++ result
                   loop
