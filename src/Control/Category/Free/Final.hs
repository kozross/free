{-# OPTIONS_GHC -Werror #-}
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
-- Final encoding of free Categories.
----------------------------------------------------------------------------
module Control.Category.Free.Final where

import Control.Category (Category (id, (.)))
import Data.Profunctor.Monad (ProfunctorFunctor (promap))
import Data.Semigroupoid (Semigroupoid (o))
import Prelude hiding (id, (.))

-- | The free 'Category' for @p@.
--
-- @since 5.1.7
newtype Cat p a b = 
  Cat { _runCat :: forall q . (Category q) => (forall x y . p x y -> q x y) -> q a b }

-- | @since 5.1.7
instance Semigroupoid (Cat p) where
  {-# INLINEABLE o #-}
  -- | This is \(\Theta(1)\).
  --
  -- @since 5.1.7
  Cat f `o` Cat g = Cat $ \nt -> f nt . g nt

-- | @since 5.1.7
instance Category (Cat p) where
  {-# INLINEABLE id #-}
  -- | @since 5.1.7
  id = Cat (const id)
  {-# INLINEABLE (.) #-}
  -- | This is \(\Theta(1)\).
  --
  -- @since 5.1.7
  Cat f . Cat g = Cat $ \nt -> f nt . g nt

-- | @since 5.1.7
instance ProfunctorFunctor Cat where
  {-# INLINEABLE promap #-}
  -- | Identical to 'hoistCat'.
  --
  -- @since 5.1.7
  promap = hoistCat

-- | Given a natural transformation from @p@ to @q@, this gives a canonical
-- \'interpretation\' of a free 'Category' of @p@ into @q@.
--
-- @since 5.1.7
runCat :: (Category q) => (forall x y . p x y -> q x y) -> Cat p a b -> q a b
runCat nt (Cat f) = f nt

-- | \'Lifts\' a @p@ into its free 'Category'.
--
-- @since 5.1.7
liftCat :: p a b -> Cat p a b
liftCat p = Cat $ \nt -> nt p

-- | Given a natural transformation from @p@ to @r@, we can \'reinterpret\' the
-- free 'Category' for @p@ as a free 'Category' for @r@.
--
-- This is \(\Theta(1)\).
--
-- @since 5.1.7
hoistCat :: (forall x y . p x y -> r x y) -> Cat p a b -> Cat r a b
hoistCat nt (Cat f) = Cat $ \nt' -> f (nt' . nt)
