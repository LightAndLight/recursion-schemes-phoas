{-# language FlexibleContexts  #-}
{-# language GADTs  #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module TC where

import Control.Lens.Fold ((^?))
import Control.Lens.Review ((#))
import Data.Functor.Foldable (Fix(..))
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
   . Ctor1 ts TyF
  => Mode ts res
  -> (a -> Ty ts)
  -> Exp a
  -> res
tc mode ctx (Exp a) =
  case mode of
    Infer -> inferInner a
    Check -> checkInner a
  where
    checkInner :: Fix (ExpF (Ty ts) a) -> Ty ts -> Bool
    checkInner (Fix e) (Fix ty) =
      case e of
        NameF{} -> inferInner (Fix e) == Just (Fix ty)
        IntF{} -> inferInner (Fix e) == Just (Fix ty)
        AddF{} -> inferInner (Fix e) == Just (Fix ty)
        AppF{} -> inferInner (Fix e) == Just (Fix ty)
        VarF{} -> inferInner (Fix e) == Just (Fix ty)
        LamF f ->
          case ty ^? _Ctor1 @TyF of
            Just (TyArrF t1 t2) -> checkInner (f t1) t2
            _ -> False

    inferInner :: Fix (ExpF (Ty ts) a) -> Maybe (Ty ts)
    inferInner (Fix e) =
      case e of
        NameF n -> Just $ ctx n
        IntF{} -> Just tyInt
        AddF a b ->
          if checkInner a tyInt && checkInner b tyInt
          then Just tyInt
          else Nothing
        AppF a b -> do
          Fix a' <- inferInner a
          a'' <- a' ^? _Ctor1 @TyF
          case a'' of
            TyArrF t1 t2 | checkInner b t1 -> Just t2
            _ -> Nothing
        VarF v -> Just v
        LamF f -> Nothing

checkScheme :: Exp a -> TypeScheme -> Bool
checkScheme e (Forall a) = _
