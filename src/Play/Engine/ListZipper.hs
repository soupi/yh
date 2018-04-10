{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable #-}

module Play.Engine.ListZipper where

import GHC.Generics
import Control.DeepSeq

data ListZipper a
  = ListZipper [a] !a [a]
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, NFData)


get :: ListZipper a -> a
get (ListZipper _ x _) = x

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

diffMapM :: Applicative f => (a -> f b) -> (a -> f b) -> ListZipper a -> f (ListZipper b)
diffMapM restF currF (ListZipper prev curr next) =
  ListZipper
    <$> traverse restF prev
    <*> currF curr
    <*> traverse restF next
