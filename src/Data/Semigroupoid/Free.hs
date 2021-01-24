{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

module Data.Semigroupoid.Free where

import Data.Profunctor (Profunctor (dimap, rmap, lmap))
import Data.Profunctor.Monad (ProfunctorFunctor (promap))
import Data.Semigroupoid (Semigroupoid (o))
import Type.Reflection (Typeable)

data Semig p a b where
  Lift :: p a b -> Semig p a b
  O :: p b c -> Semig p a b -> Semig p a c
  deriving stock (Typeable)

instance (Profunctor p) => Functor (Semig p a) where
  {-# INLINEABLE fmap #-}
  fmap f = \case
    Lift p -> Lift . rmap f $ p
    O p semig' -> O (rmap f p) semig'

instance (Profunctor p) => Profunctor (Semig p) where
  {-# INLINEABLE dimap #-}
  dimap f g = \case
    Lift p -> Lift . dimap f g $ p
    O p semig' -> O (rmap g p) . lmap f $ semig'
  {-# INLINEABLE lmap #-}
  lmap f = \case
    Lift p -> Lift . lmap f $ p
    O p semig' -> O p . lmap f $ semig'
  {-# INLINEABLE rmap #-}
  rmap g = \case
    Lift p -> Lift . rmap g $ p
    O p semig' -> O (rmap g p) semig'

instance Semigroupoid (Semig p) where
  {-# INLINEABLE o #-}
  semig `o` semig' = case semig of
    Lift p -> O p semig'
    O p semig'' -> O p (semig'' `o` semig')

instance ProfunctorFunctor Semig where
  {-# INLINEABLE promap #-}
  promap nt = \case
    Lift p -> Lift . nt $ p
    O p semig -> O (nt p) . promap nt $ semig

runSemig :: (Semigroupoid q) => (forall x y . p x y -> q x y) -> Semig p a b -> q a b
runSemig nt = \case
  Lift p -> nt p
  O p semig -> (nt p) `o` (runSemig nt semig)

liftSemig :: p a b -> Semig p a b
liftSemig = Lift

hoistSemig :: (forall x y . p x y -> q x y) -> Semig p a b -> Semig q a b
hoistSemig nt = \case
  Lift p -> Lift . nt $ p
  O p semig -> O (nt p) (hoistSemig nt semig)
