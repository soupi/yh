
-- |
-- a utility module
module Play.Utils where

import Prelude hiding (head)
import qualified Data.List as List
import Control.Lens (over, (^.))
import Data.List (group, sort)
import Data.Function ((&))

import Play.Types (Point)
import Play.Types

-- |
-- replicate operation and chain it
replicateMChain :: Monad m => Int -> (a -> m a) -> a -> m a
replicateMChain n f x
  | n <= 0 = return x
  | otherwise = f x >>= replicateMChain (n-1) f

-- |
-- if Maybe b is nothing, replace it with Left a. otherwise: Right b
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just y) = Right y
maybeToEither x Nothing  = Left x


-- |
-- returns the duplicates of a list
duplicates :: Ord a => [a] -> [a]
duplicates = map List.head . filter ((>1) . length) . group . sort

-- |
-- split arguments by element
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy v vs = map reverse $ go [] vs
  where go xs [] = [xs]
        go xs (y:ys)
          | y == v    = xs : go [] ys
          | otherwise = go (y:xs) ys

supplyBoth :: (a -> b -> c) -> (b -> a) -> b -> c
supplyBoth = (=<<)

-- | Compose with an input function that takes two arguments
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g x y = f (g x y)

----------------
-- Formatting
---------------

addPoint :: Point -> Point -> Point
addPoint = apToPoint (+)

mulPoint :: Point -> Point -> Point
mulPoint = apToPoint (*)

apToPoint :: (Int -> Int -> Int) -> Point -> Point -> Point
apToPoint f !p1 !p2 =
  p1 & over pX (f (p2 ^. pX))
     & over pY (f (p2 ^. pY))



-----------
-- Stack --
-----------

data Stack a = Stack !a ![a]

head :: Stack a -> a
head = fst . pop

push :: a -> Stack a -> Stack a
push !new (Stack !x xs) = Stack new (x:xs)

init :: a -> Stack a
init = flip Stack []

pop :: Stack a -> (a, Maybe (Stack a))
pop = \case
  Stack !x [] -> (x, Nothing)
  Stack !x (!y:ys) -> (x `seq` x, pure $! Stack y ys)

replace :: a -> Stack a -> Stack a
replace x = \case
  Stack _ xs -> Stack (x `seq` x) xs
