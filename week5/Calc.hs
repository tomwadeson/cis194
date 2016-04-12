{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

eval :: ExprT -> Integer
eval (Lit x)           = x
eval (Add expr1 expr2) = (eval expr1) + (eval expr2)
eval (Mul expr1 expr2) = (eval expr1) * (eval expr2)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul
