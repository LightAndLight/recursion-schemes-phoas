{-# language BangPatterns #-}
{-# language DeriveFunctor #-}
{-# language InstanceSigs #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
module Exp where

import Data.Functor.Foldable (Fix(..), cata)

data ExpF v n a
  = NameF n
  | VarF v
  | IntF !Int
  | AddF a a
  | AppF a a
  | LamF (v -> a)
  deriving Functor

newtype Exp a = Exp { unExp :: forall v. Fix (ExpF v a) }

instance Functor Exp where
  fmap :: forall a b. (a -> b) -> Exp a -> Exp b
  fmap f (Exp a) = Exp (go a)
    where
      go :: forall v. Fix (ExpF v a) -> Fix (ExpF v b)
      go =
        cata $
        Fix .
        \case
          NameF n -> NameF $ f n
          VarF v -> VarF v
          IntF n -> IntF n
          AddF a b -> AddF a b
          AppF a b -> AppF a b
          LamF f -> LamF f

instance Foldable Exp where
  foldMap f (Exp a) = go a
    where
      go = cata $ \case
        NameF n -> f n
        VarF v -> mempty
        IntF n -> mempty
        AddF a b -> a <> b
        AppF a b -> a <> b
        LamF f -> f ()

abstract :: Eq n => n -> Fix (ExpF v n) -> (v -> Fix (ExpF v n))
abstract _ (Fix (IntF n)) = \_ -> Fix $ IntF n
abstract _ (Fix (VarF v)) = \_ -> Fix $ VarF v
abstract n (Fix (NameF n'))
  | n == n' = \v -> Fix $ VarF v
  | otherwise = \_ -> Fix $ NameF n'
abstract n (Fix (AppF a b)) = \v -> Fix $ AppF (abstract n a v) (abstract n b v)
abstract n (Fix (AddF a b)) = \v -> Fix $ AddF (abstract n a v) (abstract n b v)
abstract n (Fix (LamF f)) = \v -> Fix $ LamF (($ v) . abstract n . f)

lam :: Eq n => n -> Fix (ExpF v n) -> Fix (ExpF v n)
lam n a = Fix . LamF $ abstract n a

app :: Fix (ExpF v n) -> Fix (ExpF v n) -> Fix (ExpF v n)
app a b = Fix $ AppF a b

add :: Fix (ExpF v n) -> Fix (ExpF v n) -> Fix (ExpF v n)
add a b = Fix $ AddF a b

var :: v -> Fix (ExpF v n)
var v = Fix $ VarF v

int :: Int -> Fix (ExpF v n)
int a = Fix $ IntF a

name :: n -> Fix (ExpF v n)
name a = Fix $ NameF a

showExp :: forall a. Show a => Exp a -> String
showExp (Exp l) = go 0 l
  where
    go :: Int -> Fix (ExpF Int a) -> String
    go !depth (Fix e) =
      case e of
        IntF n -> "IntF " <> show n
        NameF n -> "NameF " <> show n
        VarF n -> "VarF " <> show (depth-n)
        AppF a b ->
          "AppF " <>
          "(" <> go depth a <> ") (" <>
          go depth b <> ")"
        AddF a b ->
          "AddF " <>
          "(" <> go depth a <> ") (" <>
          go depth b <> ")"
        LamF f -> "LamF (" <> go (depth+1) (f (depth+1)) <> ")"
