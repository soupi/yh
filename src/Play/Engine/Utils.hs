{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- a utility module
module Play.Engine.Utils where

import qualified SDL
import qualified Foreign.C.Types as C (CInt)
import qualified Linear
import qualified Linear.Affine as Linear
import Control.Lens
import Control.DeepSeq

import Prelude hiding (head)
import qualified Data.List as List
import Control.Lens (over, (^.))
import Data.List (group, sort)
import Data.Function ((&))
import qualified Data.DList as DL

import Play.Engine.Types


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

absPoint :: IPoint -> IPoint
absPoint = fmap abs

addPoint :: Num a => Point a -> Point a -> Point a
addPoint = apToPoint (+)

mulPoint :: Num a => Point a -> Point a -> Point a
mulPoint = apToPoint (*)

apToPoint :: (a -> a -> a) -> Point a -> Point a -> Point a
apToPoint f !p1 !p2 =
  p1 & over x (f (p2 ^. x))
     & over y (f (p2 ^. y))

updateList :: (a -> [a]) -> DL.DList a -> DL.DList a
updateList f = DL.foldr (\x acc -> DL.fromList (f x) `DL.append` acc) DL.empty

updateListWith :: NFData b => b -> (b -> b -> b) -> (a -> ([a], b)) -> DL.DList a -> (DL.DList a, b)
updateListWith start combine f = flip DL.foldr (DL.empty, start) $ \x !acc ->
  case (f x, acc) of
    (([], b), (aacc, bacc)) -> (aacc, force $ combine bacc b)
    ((xs, b), (aacc, bacc)) -> (DL.fromList xs `DL.append` aacc, force $ combine bacc b)


toRect :: IPoint -> Size -> SDL.Rectangle C.CInt
toRect posi sz =
  SDL.Rectangle
    (Linear.P . uncurry Linear.V2 . over both fromIntegral . pointToTuple $ posi)
    (uncurry Linear.V2 . over both fromIntegral . sizeToTuple $ sz)

data HasPosSize
  = HasPosSize
  { _pos :: !IPoint
  , _size :: !Size
  }

makeFieldsNoPrefix ''HasPosSize

isTouching :: (HasSize a Size, HasPos a IPoint, HasSize b Size, HasPos b IPoint) => a -> b -> Maybe (a,b)
isTouching a b =
  let
    getCenter p = (p ^. pos) `addPoint` Point ((p ^. size . x) `div` 2) ((p ^. size . y) `div` 2)
    aCenter = getCenter a
    bCenter = getCenter b
    aRadius = (a ^. size . x) `div` 2
    bRadius = (b ^. size . x) `div` 2
    distX = (aCenter ^. x) - (bCenter ^. x)
    distY = (aCenter ^. y) - (bCenter ^. y)
    dist = sqrt $ fromIntegral ((distX * distX) + (distY * distY))
  in
    if dist < fromIntegral (aRadius + bRadius)
      then Just (a, b)
      else Nothing

fixPos :: (HasSize a Size, HasPos a IPoint) => Size -> a -> a
fixPos wsize entity =
  let
    x_ = entity ^. pos . x
    y_ = entity ^. pos . y
    x' =
      if x_ <= 0
        then 0
        else if x_ + (entity ^. size . x) <= (wsize ^. x)
          then x_
          else (wsize ^. x) - (entity ^. size . x)
    y' =
      if y_ <= 0
        then 0
        else if y_ + (entity ^. size . y) <= (wsize ^. y)
          then y_
          else (wsize ^. y) - (entity ^. size . y)
  in
    set pos (Point x' y') entity


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
