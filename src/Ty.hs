{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}
module Ty where

import Data.Deriving (deriveEq1)
import Data.Functor.Foldable (Fix(..))
import Data.Functor.Sum (Sum(..))
import Data.Variant1 (Variant1)

newtype TypeScheme = Forall { unForall :: forall v. Fix (Variant1 '[TyVarF v, TyF]) }
newtype TyVarF v a = TyVarF v
  deriving (Eq, Show, Functor)
data TyF a
  = TyArrF a a
  | TyUnitF
  | TyIntF
  deriving (Eq, Show, Functor)
deriveEq1 ''TyF
