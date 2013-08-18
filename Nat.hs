module Nat where

data Nat = Z | S Nat

instance Show Nat where
  show Z = show 0
  show (S n) = let m = ((read (show n)) :: Int)
               in show (m + 1)
zero = Z
one = S zero
two = S one
three = S two
four = S three

instance Eq Nat where
  Z == Z = True
  Z == (S n) = False
  (S a) == (S b) = a == b

instance Ord Nat where
  (S n) > Z = True
  Z > (S n) = False
  (S a) > (S b) = a > b

  (S n) < Z = False
  Z < (S n) = True
  (S a) < (S b) = a < b

  Z <= _ = True
  (S n) <= Z = False
  (S a) <= (S b) = a <= b

  _ >= Z = True
  Z >= (S n) = False
  (S a) >= (S b) = a >= b

