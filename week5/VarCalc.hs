{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module VarCalc where

import Control.Applicative (liftA2)
import qualified Data.Map as M

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: String -> a

data VarExprT = Var String
              | Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
  deriving (Eq, Show)

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x   = \_ -> Just x
  add x y = \m -> liftA2 (+) (x m) (y m)
  mul x y = \m -> liftA2 (*) (x m) (y m)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
