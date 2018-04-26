{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}

module CategoricDefinitions where

import GHC.Exts (Constraint)

class NoConstraint a where

instance NoConstraint a where

class Category k where
  type Allowed k :: * -> Constraint
  type Allowed k = NoConstraint

  id  :: Allowed k a => a `k` a
  (.) :: (Allowed k a, 
          Allowed k b, 
          Allowed k c) => (b `k` c) -> (a `k` b) -> (a `k` c)

class Category k => Monoidal k where
  x :: (Allowed k a,
        Allowed k b,
        Allowed k c,
        Allowed k d,
        Allowed k (a, b),
        Allowed k (c, d)) => (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d)) 

class Monoidal k => Cartesian k where
  exl :: (Allowed k (a, b),
          Allowed k a) => (a, b) `k` a
  exr :: (Allowed k (a, b),
          Allowed k b) => (a, b) `k` b
  dup :: (Allowed k (a, a),
          Allowed k a) => a `k` (a, a)

class Category k => Cocartesian k where
  inl :: (Allowed k a,
          Allowed k b) => a `k` (a, b)
  inr :: (Allowed k a,
          Allowed k b) => b `k` (a, b)
  jam :: (Allowed k a,
          Allowed k (a, a)) => (a, a) `k` a

--------------------------------------

class NumCat k a where
  negateC :: a `k` a
  addC :: (a, a) `k` a
  mulC :: (a, a) `k` a

class Scalable k a where
  scale :: a -> (a `k` a)
