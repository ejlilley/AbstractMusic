module MusicGraph where

import Util (foundIn, uniq)
import Data.List


-- import Music (Name(..), Accidental(..),
--               Quality(..), Number(..), Ratio(..),
--               sharp, flat, natural, pitch, int, note, relnote,
--               crotchet, minim, semibreve, quaver,
--               AbstractPitch2(..), AbstractInt2(..), AbstractDur2(..),
--               AbstractNote(..), Note)
-- 
-- 
-- x = note (pitch A sharp) minim
-- y = note (pitch G flat) quaver
-- 
-- z = relnote (int Dim Fourth) crotchet




data Graph a = a :~> [Graph a]

instance (Show a, Eq a) => Show (Graph a) where
  show g = traverseShow [] g

showNode a = "{" ++ (show a) ++ "}"

joinStrings _ [] = ""
joinStrings _ (s:[]) = s
joinStrings k (s:ss) = s ++ k ++ (joinStrings k ss)

traverseShow :: (Eq a, Show a) => [a] -> Graph (a) -> [Char]
traverseShow seen (a :~> succ) = if a `foundIn` seen
                                 then showNode a
                                 else (showNode a) ++ ":~>[" ++ (joinStrings ", " $ map (traverseShow (a:seen)) succ) ++ "]"

instance Functor Graph where
  fmap f (a :~> succ) = (f a) :~> (map (fmap f) succ)


traverse :: (Eq a) => (a -> a -> a) -> a -> [a] -> (Graph a) -> a
traverse f e seen (n :~> succ) = if n `foundIn` seen
                                 then e
                                 else foldl f n $ map (traverse f e (n:seen)) succ

mapNodes f (n :~> s) = let (n' :~> s') = f (n :~> s)
                       in n' :~> (map (mapNodes f) s')


names :: Eq a => Graph a -> [a]
names g = uniq $ names' [] g
  where names' seen (n :~> succ) = if n `foundIn` seen
                                   then []
                                   else n:(concatMap (names' (n:seen)) succ)

nodes :: Eq a => (Graph a) -> [Graph a]
nodes g = nodes' [] g
  where nodes' seen (n :~> s) = if n `foundIn` seen
                                then []
                                else (n :~> s):(concatMap (nodes' (n:seen)) s)

name (n :~> _) = n
succ (_ :~> s) = s

-- successors :: (Eq a) => [a] -> a -> Graph a -> [a]
-- successors seen x (n :~> s)
--   | n `foundIn` seen = []
--   | x `foundIn` (map node s) = n:(concatMap (successors (n:seen) x) s)
--   | otherwise = concatMap (successors (n:seen) x) s

successors x g = case findNode x g of
  Just (n :~> s) -> map name s
  Nothing -> []


modifyNode f x = mapNodes (\(n :~> s) -> if n == x then f (n :~> s) else n :~> s)

first _ [] = Nothing
first f (x:xs)
  | f x = Just x
  | otherwise = first f xs


findNode x g = first (\(n :~> _) -> n == x) (nodes g)


addEdge x y g = case findNode y g of
  Just y' -> modifyNode (\(n :~> s) -> n :~> (y':s)) x g
  Nothing -> g

-- delEdge x y = modifyNode (\(n :~> s) -> n :~> ()) x


a = 4 :~> [b,d]
b = 5 :~> [c]
c = 6 :~> [f]
d = 1 :~> [e]
e = 2 :~> [f]
f = 3 :~> []

mergeNode g (n' :~> []) = g
mergeNode g (n' :~> (x:xs)) = mergeNode (addEdge n' (name x) g) (n' :~> xs)

mergeGraphs a b = mergeGraphs' a (nodes b) where
  mergeGraphs' g [] = g
  mergeGraphs' g (h:hs) = mergeGraphs' (mergeNode g h) hs

reverseNode g (n :~> s) = let n' = n :~> []
                              rev = map (\x -> (name x) :~> [n']) s
                          in foldr mergeNode g rev

--uniqUnder = 

-- transpose (n :~> s) = 


