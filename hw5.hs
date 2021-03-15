
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

    
import ExprT
import StackVM
import Parser
import Data.Maybe
import qualified Data.Map as M


eval :: ExprT -> Integer
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Lit n) = n
eval (ExprT.Mul x y) = eval x * eval y


--data Maybe a = Just a | Nothing

--exercise 2
evalStr::String -> Maybe Integer
--evalStr = fmap eval . parseExp Lit Add Mul
evalStr str = case (parseExp ExprT.Lit ExprT.Add ExprT.Mul str) of
                (Just a) -> Just (eval a) 
                Nothing -> Nothing


--exercise 3
class Expr a where 
    lit :: Integer -> a
    add :: a-> a ->a 
    mul :: a-> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id


--exercise 4
instance Expr Integer where
    lit = id 
    add = (+)
    mul = (*)

instance Expr Bool where
    lit x 
        | x <= 0 = False
        | otherwise = True 
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)


instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y)= MinMax (max x y)
    mul (MinMax x) (MinMax y)= MinMax (min x y)  

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y)= Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y)= Mod7 ((x * y) `mod` 7)

-- tests
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp

--exercise 5

instance Expr StackVM.Program where
    lit i = [StackVM.PushI i]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]


compile :: String -> Maybe Program
compile = parseExp lit add mul 


--exercise 6
class HasVars a where
    var :: String -> a



data VarExprT =  VarExprT String Integer
  deriving (Show, Eq)

instance HasVars VarExprT where
    var  str = VarExprT str 0

instance Expr VarExprT where
    lit a = VarExprT  "" a
    add (VarExprT _ a) (VarExprT _ b) = VarExprT "" (a+b)  
    mul (VarExprT _ a) (VarExprT _ b) = VarExprT "" (a * b)


type MapExpr = M.Map String Integer -> Integer

instance HasVars MapExpr where
    var = M.lookup

instance Expr MapExpr where
    lit a = (\_ -> Just a)
    add f g = \m -> case (isNothing(f m) || isNothing(g m)) of 
                        True -> Nothing
                        _    -> Just (fromJust(f m) + fromJust(g m))
    mul f g = \m -> case (isNothing(f m) || isNothing (g m)) of
                        True -> Nothing
                        _    -> Just (fromJust(f m) * fromJust( g m))


withVars :: [(String,Integer)] -> MapExpr -> Maybe Integer
withVars vs expr = expr $ M.fromList vs

