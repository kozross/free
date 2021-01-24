{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

module Data.Semigroupoid.Free.Final where

import Data.Profunctor.Monad (ProfunctorFunctor (promap))
import Data.Semigroupoid (Semigroupoid (o))

newtype Semig p a b = 
  Semig { _runSemig :: forall q . (Semigroupoid q) => (forall x y . p x y -> q x y) -> q a b }

instance Semigroupoid (Semig p) where
  {-# INLINEABLE o #-}
  Semig f `o` Semig g = Semig $ \k -> f k `o` g k

instance ProfunctorFunctor Semig where
  {-# INLINEABLE promap #-}
  promap = hoistSemig

runSemig :: (Semigroupoid q) => (forall x y . p x y -> q x y) -> Semig p a b -> q a b
runSemig nt (Semig f) = f nt

liftSemig :: p a b -> Semig p a b
liftSemig p = Semig $ \nt -> nt p

hoistSemig :: (forall x y . p x y -> q x y) -> Semig p a b -> Semig q a b
hoistSemig nt (Semig f) = Semig $ \k -> f (k . nt)
