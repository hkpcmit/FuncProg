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

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add n m)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)
