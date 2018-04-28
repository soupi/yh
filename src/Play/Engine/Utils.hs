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
import qualified Data.DList as DL

import Play.Engine.Types

-- import Debug.Trace

firstM :: Functor f => (a -> f c) -> (a, b) -> f (c, b)
firstM f (a, b) = (, b) <$> f a

-- |
-- replicate operation and chain it
replicateMChain :: Monad m => Int -> (a -> m a) -> a -> m a
replicateMChain n f x'
  | n <= 0 = return x'
  | otherwise = f x' >>= replicateMChain (n-1) f

-- |
-- if Maybe b is nothing, replace it with Left a. otherwise: Right b
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just y') = Right y'
maybeToEither x' Nothing  = Left x'


-- |
-- returns the duplicates of a list
duplicates :: Ord a => [a] -> [a]
duplicates = map List.head . filter ((>1) . length) . group . sort

-- |
-- split arguments by element
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy v vs = map reverse $ go [] vs
  where go ws [] = [ws]
        go ws (z:zs)
          | z == v    = ws : go [] zs
          | otherwise = go (z:ws) zs

supplyBoth :: (a -> b -> c) -> (b -> a) -> b -> c
supplyBoth = (=<<)

-- | Compose with an input function that takes two arguments
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g a b = f (g a b)

absPoint :: IPoint -> IPoint
absPoint = fmap abs

addPoint :: (NFData a, Num a) => Point a -> Point a -> Point a
addPoint (Point x1 y1) (Point x2 y2) = force $ Point (x1 + x2) (y1 + y2)
{-# INLINE addPoint #-}

mulPoint :: (NFData a, Num a) => Point a -> Point a -> Point a
mulPoint (Point x1 y1) (Point x2 y2) = force $ Point (x1 * x2) (y1 * y2)
{-# INLINE mulPoint #-}

updateList :: (a -> [a]) -> DL.DList a -> DL.DList a
updateList f = DL.foldr (\a acc -> DL.fromList (f a) `DL.append` acc) DL.empty

updateListWith :: NFData b => b -> (b -> b -> b) -> (a -> ([a], b)) -> DL.DList a -> (DL.DList a, b)
updateListWith start combine f = flip DL.foldr (DL.empty, start) $ \a !acc ->
  case (f a, acc) of
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
    aRadius = (a ^. size . x) `div` 2 - (a ^. size . x) `div` 8
    bRadius = (b ^. size . x) `div` 2 - (b ^. size . x) `div` 8
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

isInWindow :: Size -> IPoint -> Size -> Bool
isInWindow = isInSquare (Point 0 0)

isInSquare :: IPoint -> Size -> IPoint -> Size -> Bool
isInSquare wpos wsize posi sz
  |  posi ^. x + sz ^. x < (wpos ^. x) || posi ^. x > wpos ^.x + wsize ^. x
  || posi ^. y + sz ^. y < (wpos ^. y) || posi ^. y > wpos ^.y + wsize ^. y
  = False

  | otherwise = True

isAround :: IPoint -> IPoint -> Size -> Bool
isAround place posi sz =
  isInSquare (place `addPoint` Point (-5) (-5)) sz posi sz

dirToPlace :: IPoint -> IPoint -> IPoint
dirToPlace posi place =
  Point (dir x) (negate $ dir y)
  where
    dir c
      | place ^. c > posi ^. c = 1
      | place ^. c < posi ^. c = -1
      | otherwise = 0

type Camera = IPoint -> IPoint

-----------
-- Stack --
-----------

data Stack a = Stack !a ![a]

head :: Stack a -> a
head = fst . pop

push :: a -> Stack a -> Stack a
push !new (Stack !a as) = Stack new (a:as)

init :: a -> Stack a
init = flip Stack []

pop :: Stack a -> (a, Maybe (Stack a))
pop = \case
  Stack !a [] -> (a, Nothing)
  Stack !a (!b:bs) -> (a `seq` a, pure $! Stack b bs)

replace :: a -> Stack a -> Stack a
replace a = \case
  Stack _ as -> Stack (a `seq` a) as
