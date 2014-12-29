import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
   | Succ Nat
   deriving Show

integerToNat1 :: Integer -> Nat
integerToNat1 0 = Zero
integerToNat1 (n+1) = Succ (integerToNat1 n)
 
integerToNat5 :: Integer -> Nat
integerToNat5 (n+1) = Succ (integerToNat5 n)
integerToNat5 0 = Zero

integerToNat6 :: Integer -> Nat
integerToNat6 (n+1) = let m = integerToNat6 n in Succ m
integerToNat6 0 = Zero
