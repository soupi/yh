{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable #-}

module Play.Engine.ListZipper where

import GHC.Generics
import Control.DeepSeq

data ListZipper a
  = ListZipper [a] !a [a]
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Generic, NFData)

instance Traversable ListZipper where
  traverse f (ListZipper p c n) =
    ListZipper
      <$> fmap reverse (traverse f (reverse p))
      <*> f c
      <*> traverse f n

get :: ListZipper a -> a
get (ListZipper _ x _) = x

overCurr :: (a -> a) -> ListZipper a -> ListZipper a
overCurr f (ListZipper p x n) = ListZipper p (f x) n

nextStop :: ListZipper a -> ListZipper a
nextStop = \case
  ListZipper prev curr (n:next) ->
    ListZipper (curr : prev) n next
  l -> l

nextCycle :: ListZipper a -> ListZipper a
nextCycle = \case
  ListZipper prev curr (n:next) ->
    ListZipper (curr : prev) n next
  ListZipper prev curr [] ->
    let
      (curr' : next) = reverse $ curr : prev
    in
      ListZipper [] curr' next


prevCycle :: ListZipper a -> ListZipper a
prevCycle = \case
  ListZipper (p:prev) curr next ->
    ListZipper prev p (curr : next)
  ListZipper [] curr next ->
    let
      (curr' : prev) = reverse $ curr : next
    in
      ListZipper prev curr' []

first :: ListZipper a -> a
first = \case
  ListZipper [] curr _ ->
    curr
  ListZipper prev _ _ ->
    Prelude.last prev

last :: ListZipper a -> a
last = \case
  ListZipper _ curr [] ->
    curr
  ListZipper _ _ next ->
    Prelude.last next

diffMapM :: Applicative f => (a -> f b) -> (a -> f b) -> ListZipper a -> f (ListZipper b)
diffMapM restF currF (ListZipper prev curr next) =
  ListZipper
    <$> traverse restF prev
    <*> currF curr
    <*> traverse restF next

addIndex :: ListZipper a -> ListZipper (Int, a)
addIndex = \case
  ListZipper prev curr next ->
    ListZipper
      (reverse $ zip [0..] $ reverse prev)
      (length prev, curr)
      (zip [length prev + 1..] next)
