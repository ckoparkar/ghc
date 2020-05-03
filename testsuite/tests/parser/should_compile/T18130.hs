{-# Language DerivingVia #-}
{-# Language KindSignatures #-}

module T18130 where

import Data.Functor.Classes
import Data.Kind

newtype Par a b = Par (a, b)
  deriving Eq
  via (a, b)
   :: Type

  deriving Eq1
  via (,) a
   :: Type -> Type

  deriving Eq2
  via (,)
   :: Type -> Type -> Type
