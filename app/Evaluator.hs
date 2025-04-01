module Evaluator where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Text.Read (readMaybe)

-- Expression AST
data Expr
  = Const Double -- Literal number
  | Add Expr Expr -- (+ a b)
  | Sub Expr Expr -- (- a b)
  | Mul Expr Expr -- (* a b)
  | Div Expr Expr -- (/ a b)

  deriving (Show, Eq)

operatorSymbols :: [Char]
operatorSymbols = "+-*/"

bracesSymbols :: [Char]
bracesSymbols = "()"

tokenize :: String -> [String]
tokenize = go []
  where
    go acc [] = reverse acc
    go acc (c : cs)
      | isSpace c = go acc cs
      | c `elem` (operatorSymbols ++ bracesSymbols) =
          go ([c] : acc) cs
      | isDigit c || isAlpha c || c == '.' =
          let (tok, rest) = span isIdentChar (c : cs)
           in go (tok : acc) rest
      | otherwise = go acc cs

    isIdentChar x = isAlphaNum x || x == '.'

-- Operator lookup table
lookupOp :: String -> Maybe (Expr -> Expr -> Expr)
lookupOp "+" = Just Add
lookupOp "-" = Just Sub
lookupOp "*" = Just Mul
lookupOp "/" = Just Div
lookupOp _ = Nothing

-- Recursive parser for fully parenthesized prefix expressions
parseExpr :: [String] -> Maybe (Expr, [String])
parseExpr [] = Nothing
parseExpr ("(" : tok : rest)
  | Just op <- lookupOp tok = do
      (lhs, rest1) <- parseExpr rest
      (rhs, rest2) <- parseExpr rest1
      case rest2 of
        (")" : rest3) -> return (op lhs rhs, rest3)
        _ -> Nothing -- missing closing paren
parseExpr (tok : rest)
  | Just n <- readMaybe tok = Just (Const n, rest)
parseExpr _ = Nothing

-- Parse expression from raw string
readExpr :: String -> Maybe Expr
readExpr s = case parseExpr (tokenize s) of
  Just (expr, []) -> Just expr
  _ -> Nothing

evalNode :: Expr -> Maybe Double
evalNode (Const d) = Just d
evalNode (Add l r) = do
  lv <- evalNode l
  rv <- evalNode r
  return (lv + rv)
evalNode (Sub l r) = do
  lv <- evalNode l
  rv <- evalNode r
  return (lv - rv)
evalNode (Mul l r) = do
  lv <- evalNode l
  rv <- evalNode r
  return (lv * rv)
evalNode (Div l r) = do
  lv <- evalNode l
  rv <- evalNode r
  if rv /= 0
    then return (lv / rv)
    else Nothing
