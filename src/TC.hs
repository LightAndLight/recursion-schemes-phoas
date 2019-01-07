{-# language BangPatterns  #-}
{-# language DataKinds  #-}
{-# language FlexibleContexts  #-}
{-# language GADTs  #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module TC where

import Control.Lens.Fold ((^?))
import Control.Lens.Review ((#), review)
import Data.Functor.Classes (Eq1)
import Data.Functor.Foldable (Fix(..), cata)
import Data.Variant1 (Variant1, Ctor1)
import Data.Variant1.Lens (_Ctor1)

import Exp
import Ty

type Ty ts = Fix (Variant1 ts)

data Mode ts res where
  Check :: Mode ts (Ty ts -> Bool)
  Infer :: Mode ts (Maybe (Ty ts))

mkTy :: Ctor1 ts TyF => TyF (Fix (Variant1 ts)) -> Fix (Variant1 ts)
mkTy a = Fix $ _Ctor1 @TyF # a

tyInt :: Ctor1 ts TyF => Fix (Variant1 ts)
tyInt = mkTy TyIntF

tc
  :: forall a res ts
   . ( Eq1 (Variant1 ts)
     , Ctor1 ts TyF
     )
  => Mode ts res
  -> (a -> Ty ts)
  -> Exp a
  -> res
tc mode ctx (Exp a) =
  case mode of
    Infer -> inferInner ctx a
    Check -> checkInner ctx a
  where
    checkInner
      :: forall ts
       . ( Eq1 (Variant1 ts)
         , Ctor1 ts TyF
         )
      => (a -> Ty ts)
      -> Fix (ExpF (Ty ts) a)
      -> Ty ts
      -> Bool
    checkInner ctx (Fix e) (Fix ty) =
      case e of
        NameF{} -> inferInner ctx (Fix e) == Just (Fix ty)
        IntF{} -> inferInner ctx (Fix e) == Just (Fix ty)
        AddF{} -> inferInner ctx (Fix e) == Just (Fix ty)
        AppF{} -> inferInner ctx (Fix e) == Just (Fix ty)
        VarF{} -> inferInner ctx (Fix e) == Just (Fix ty)
        LamF f ->
          case ty ^? _Ctor1 @TyF of
            Just (TyArrF t1 t2) -> checkInner ctx (f t1) t2
            _ -> False

    inferInner
      :: forall ts
       . ( Eq1 (Variant1 ts)
         , Ctor1 ts TyF
         )
      => (a -> Ty ts)
      -> Fix (ExpF (Ty ts) a)
      -> Maybe (Ty ts)
    inferInner ctx (Fix e) =
      case e of
        NameF n -> Just $ ctx n
        IntF{} -> Just tyInt
        AddF a b ->
          if checkInner ctx a tyInt && checkInner ctx b tyInt
          then Just tyInt
          else Nothing
        AppF a b -> do
          Fix a' <- inferInner ctx a
          a'' <- a' ^? _Ctor1 @TyF
          case a'' of
            TyArrF t1 t2 | checkInner ctx b t1 -> Just t2
            _ -> Nothing
        VarF v -> Just v
        LamF f -> Nothing

checkScheme :: forall a. (a -> Fix TyF) -> Exp a -> TypeScheme -> Bool
checkScheme ctx e (Forall a) = go 0 a
  where
    go :: Int -> ForallV Int -> Bool
    go !n (Done a) = tc Check (cata (Fix . review (_Ctor1 @TyF)) . ctx) e a
    go !n (More a) = go (n+1) $ a n
