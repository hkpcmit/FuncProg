import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
   | Succ Nat
   deriving Show

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat (n+1) = Succ (integerToNat n)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

add1 :: Nat -> Nat -> Nat
add1 Zero n = n
add1 (Succ m) n = Succ (add1 n m)

add2 :: Nat -> Nat -> Nat
add2 (Succ m) n = Succ (add2 n m)
add2 Zero n = n

add7 :: Nat -> Nat -> Nat
add7 n Zero = n
add7 n (Succ m) = Succ (add7 m n)

add8 :: Nat -> Nat -> Nat
add8 n (Succ m) = Succ (add8 m n)
add8 n Zero = n
