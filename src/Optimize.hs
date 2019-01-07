{-# language LambdaCase #-}
module Optimize where

import Data.Functor.Foldable (Fix(..))

import Exp

transform :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
transform f = go
  where
    go (Fix a) = f . Fix $ fmap go a

rewrite :: Functor f => (Fix f -> Maybe (Fix f)) -> Fix f -> Fix f
rewrite f = transform go
  where
    go a = maybe a go $ f a

foldConstants :: Exp a -> Exp a
foldConstants (Exp a) =
  Exp
  (rewrite
     (\case
         Fix (AddF (Fix (IntF n)) (Fix (IntF m))) -> Just $ int (n+m)
         _ -> Nothing)
     a)