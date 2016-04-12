{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x)           = x
eval (Add expr1 expr2) = (eval expr1) + (eval expr2)
eval (Mul expr1 expr2) = (eval expr1) * (eval expr2)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul
