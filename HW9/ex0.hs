import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
   | Succ Nat
   deriving Show

natToInteger1 :: Nat -> Integer
natToInteger1 Zero = 0
natToInteger1 (Succ n) = natToInteger1 n + 1

natToInteger2 :: Nat -> Integer
natToInteger2 (Succ n) = natToInteger2 n + 1
natToInteger2 Zero = 0

natToInteger4 :: Nat -> Integer
natToInteger4 (Succ n) = 1 + natToInteger4 n
natToInteger4 Zero = 0

natToInteger6 :: Nat -> Integer
natToInteger6 = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1 : m n)]]

natToInteger7 :: Nat -> Integer
natToInteger7 = \n -> genericLength [c | c <- show n, c == 'S']

