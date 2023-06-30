{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import ExprT ( ExprT(..) )
import Parser
import StackVM

import qualified Data.Map as M

-- EXERCISE 1
eval :: ExprT -> Integer
eval (ExprT.Lit n)         = n
eval (ExprT.Add exp1 exp2) = (eval exp1) + (eval exp2)
eval (ExprT.Mul exp1 exp2) = (eval exp1) * (eval exp2)


-- EXERCISE 2
evalParser :: String -> Maybe Integer
evalParser s = case parseExp ExprT.Lit ExprT.Add ExprT.Mul s of
                (Just s) -> Just (eval s)
                Nothing -> Nothing

-- EXERCISE 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul


-- EXERCISE 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit n
        | n <= 0    = False
        | otherwise = True
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax m) (MinMax n) = MinMax $ max m n
    mul (MinMax m) (MinMax n) = MinMax $ min m n

instance Expr Mod7 where
    lit n = Mod7 (n `mod` 7)
    add (Mod7 m) (Mod7 n) = Mod7 $ (n + m) `mod` 7
    mul (Mod7 m) (Mod7 n) = Mod7 $ (n * m) `mod` 7

-- Testing
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- EXERCISE 5
instance Expr Program where
    lit n = [StackVM.PushI n]
    add s1 s2 = s1 ++ s2 ++ [StackVM.Add]
    mul s1 s2 = s1 ++ s2 ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- EXERCISE 6
class HasVars a where
    var :: String -> a


data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = Main.Lit
    add = Main.Add
    mul = Main.Mul

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n _             = Just n
    add var0 var1 map0 = do int0 <- var0 map0
                            int1 <- var1 map0
                            return (int0 + int1)
    mul var0 var1 map0 = do int0 <- var0 map0
                            int1 <- var1 map0
                            return (int0 * int1)

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
