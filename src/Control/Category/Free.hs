{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Category.Free
-- Copyright   :  (C) 2021 Koz Ross
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Koz Ross <koz.ross@retro-freedom.nz>
-- Stability   :  provisional
-- Portability :  GADTs, RankNTypes
--
-- Categories for free. 
----------------------------------------------------------------------------
module Control.Category.Free where

import Control.Category (Category (id, (.)))
import Data.Kind (Type)
import Type.Reflection (Typeable)
import Data.Semigroupoid (Semigroupoid (o))
import Prelude hiding (id, (.))

-- | The free 'Category' for @p@.
--
-- @since 5.1.7 
data Cat (p :: Type -> Type -> Type) a b where
  -- | @since 5.1.7
  Id :: Cat p a a
  -- | @since 5.1.7
  Dot :: p b c -> Cat p a b -> Cat p a c
  deriving stock (Typeable)

-- | @since 5.1.7
instance Semigroupoid (Cat p) where
  {-# INLINEABLE o #-}
  -- | This is \(\Theta(n^2)\), where \(n)\ is the number of @p@s in the result.
  --
  -- @since 5.1.7
  cat `o` cat' = cat . cat'

-- | @since 5.1.7
instance Category (Cat p) where
  {-# INLINEABLE id #-}
  -- | @since 5.1.7
  id = Id
  {-# INLINEABLE (.) #-}
  -- | This is \(\Theta(n^2)\), where \(n)\ is the number of @p@s in the result.
  --
  -- @since 5.1.7
  cat . cat' = case cat of
    Id -> cat'
    Dot p cat'' -> case cat' of
      Id -> cat
      Dot{} -> Dot p (cat'' . cat')

-- | Given a natural transformation from @p@ to @q@, this gives a canonical
-- \'interpretation\' of a free 'Category' of @p@ into @q@.
--
-- This is \(\Theta(n)\), where \(n\) is the number of @p@s in the input.
--
-- @since 5.1.7
{-# INLINEABLE runCat #-}
runCat :: (Category q) => (forall x y . p x y -> q x y) -> Cat p a b -> q a b
runCat nt = \case
  Id -> id
  Dot p cat' -> nt p . (runCat nt cat')

-- | \'Lifts\' a @p@ into its free 'Category'.
--
-- @since 5.1.7
liftCat :: p a b -> Cat p a b
liftCat p = Dot p Id

-- | Given a natural transformation from @p@ to @q@, we can \'reinterpret\' the
-- free 'Category' for @p@ as a free 'Category' for @q@.
--
-- @since 5.1.7
hoistCat :: (forall x y . p x y -> q x y) -> Cat p a b -> Cat q a b
hoistCat nt = \case
  Id -> Id
  Dot p cat' -> Dot (nt p) (hoistCat nt cat')
