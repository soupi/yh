{-# LANGUAGE PatternSynonyms #-}

module Play.Engine.Types where

import qualified SDL.Vect as Vect

import Control.Lens

-----------
-- Types --
-----------

type Point a = Vect.V2 a
type IPoint = Vect.V2 Int
type FPoint = Vect.V2 Float
type Size = Vect.V2 Int

x :: Lens' (Vect.V2 a) a
x = lens (\(Vect.V2 a _) -> a) (\(Vect.V2 _ b) a -> (Vect.V2 a b))

y :: Lens' (Vect.V2 a) a
y = lens (\(Vect.V2 _ b) -> b) (\(Vect.V2 a _) b -> (Vect.V2 a b))

pattern Point :: a -> a -> Vect.V2 a
pattern Point x' y' = Vect.V2 x' y'

pointToTuple :: Point a -> (a, a)
pointToTuple (Point !x' !y') = (x', y')

sizeToTuple :: Size -> (Int, Int)
sizeToTuple (Vect.V2 !w !h) = (w, h)
