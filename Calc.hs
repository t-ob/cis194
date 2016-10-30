{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}


module Calc where

import qualified Data.Map as M
import Parser
import StackVM

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr Integer where
    lit i = i
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit i
        | i <= 0    = False
        | otherwise = True
    add b1 b2 = b1 || b2
    mul b1 b2 = b1 && b2

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit i = MinMax i
    add (MinMax i) (MinMax j) = MinMax (max i j)
    mul (MinMax i) (MinMax j) = MinMax (min i j)

instance Expr Mod7 where
    lit i = Mod7 (i `mod` 7)
    add (Mod7 i) (Mod7 j) = Mod7 ((i + j) `mod` 7)
    mul (Mod7 i) (Mod7 j) = Mod7 ((i * j) `mod` 7)

instance Expr Program where
    lit i = [PushI i]
    add p1 p2 = p1 ++ p2 ++ [Add]
    mul p1 p2 = p1 ++ p2 ++ [Mul]

compile :: String -> Maybe Program
compile s = parseExp lit add mul s :: Maybe Program

class HasVars a where
    var :: String -> a

data VarExprT = L Integer
              | A VarExprT VarExprT
              | M VarExprT VarExprT
              | V String
  deriving (Show, Eq)

instance HasVars VarExprT where
    var s = V s

instance Expr VarExprT where
    lit i = L i
    add e1 e2 = A e1 e2
    mul e1 e2 = M e1 e2

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var s = \m -> M.lookup s m

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit i = \_ -> Just i
    add f1 f2 = \m -> case ((f1 m), (f2 m)) of
                        (Nothing, _)       -> Nothing
                        (_, Nothing)       -> Nothing
                        (Just v1, Just v2) -> Just (v1 + v2)
    mul f1 f2 = \m -> case ((f1 m), (f2 m)) of
                        (Nothing, _)       -> Nothing
                        (_, Nothing)       -> Nothing
                        (Just v1, Just v2) -> Just (v1 * v2)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
