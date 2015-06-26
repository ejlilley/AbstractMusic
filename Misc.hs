module Misc where

import Control.Monad
import Data.Monoid

-- iterator f x = f x

-- l = iterate (iterator (+2)) 0

-- l = [0,2,4,6...]

-- l' = [0, -2, -4, -6...]

-- l' = map (*(-1)) l


-- iterateStep 0 = 1
-- iterateStep 1 = 2
-- iterateStep 2 = 3
-- iterateStep 3 = 0

recycle (x:xs) = xs ++ [x]

interleave (x:xs) (y:ys) = x:y:(interleave xs ys)

makeStep g i f (x,next) = (g x (f next), i next)

step = makeStep (+) recycle head

-- iterator :: [(Integer, Integer)]
iterator = map fst $ iterate step (0,[0,1,2,3])
iterator' = map fst $ iterate step (0,[0,-1,-2,-3])


f _ = [1,2,3]
g a = [4, a, a + 1]

-- foo = [0] >>= f >>= g

-- iterateM = liftM iterate

myIterate f x = (f x) : (myIterate f (f x))

-- iterateM :: (a -> [a]) -> [a] -> [a]
iterateM f x = (f =<< x) `mplus` (iterateM f (f =<< x))

data Pair b a = Pair [a] [b] deriving Show

pairX (Pair x y) = x
pairY (Pair x y) = y

instance Monad (Pair b) where
  (Pair xs ys) >>= f = let pairs = map f xs
                           xs' = foldl (++) [] (map pairX pairs)
                           ys' = pairY (head pairs)
                       in Pair xs' ys'
  return x = Pair [x] []

instance MonadPlus (Pair b) where
  mplus (Pair [] ys) (Pair xs' _) = Pair xs' ys
  mplus (Pair xs ys) (Pair [] _) = Pair xs ys
  mplus (Pair xs ys) (Pair xs' _) = Pair (xs ++ xs') ys
  mzero = Pair [] []

-- foo (Pair xs ys) = Pair ((\x -> [x + (head ys)]) =<< xs) (recycle ys)

-- foo :: Num b => b -> Pair b b


-- makeStep' g i f (Pair x next) = Pair (g x (f next)) (i next)

stepM = makeStep (\x -> \y -> [x + y, x - y]) recycle head

-- iteratorM = iterateM stepM (0,[0,1,2,3])

iterateM' f g h (x, y) = let x' = f =<< (h y) =<< x
                             y' = g y
                         in x' `mplus` (iterateM' f g h (x', y'))

iteratorM = iterateM' (\x -> [x]) recycle (\y -> \x -> [x ++ (head y), x ++ "1"]) (["0"], ["a","b","c","d"])



iterateM'' f g state = (f state) `mplus` (iterateM'' f g (g state))

bar = iterateM'' f g
      where f (xs, ys) = (\x -> [x + (head ys)]) =<< xs
            g (xs, ys) = (f (xs, ys), recycle ys)

