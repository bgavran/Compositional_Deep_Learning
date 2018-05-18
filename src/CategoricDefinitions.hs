{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module CategoricDefinitions where

import GHC.Exts (Constraint)

class NoConstraint a where

instance NoConstraint a where

class Category k where
  type Allowed k a :: Constraint
  type Allowed k a = ()

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
  exl :: (Allowed k a,
          Allowed k b,
          Allowed k (a, b)) => (a, b) `k` a
  exr :: (Allowed k a,
          Allowed k b,
          Allowed k (a, b)) => (a, b) `k` b
  dup :: (Allowed k (a, a),
          Allowed k a) => a `k` (a, a)

class Category k => Cocartesian k where
  inl :: (Allowed k a,
          Allowed k b,
          Allowed k (a, b)) => a `k` (a, b)
  inr :: (Allowed k a,
          Allowed k b,
          Allowed k (a, b)) => b `k` (a, b)
  jam :: (Allowed k a,
          Allowed k (a, a)) => (a, a) `k` a

--------------------------------------

class Additive a where
  zero :: a
  one :: a
  (^+) :: a -> a -> a

class NumCat k a where
  negateC :: a `k` a
  addC :: (a, a) `k` a
  mulC :: (a, a) `k` a

class Scalable k a where
  scale :: a -> (a `k` a)

-------------------------------------
-- Instances
-------------------------------------
--

instance Category (->) where
  id    = \a -> a
  g . f = \a -> g (f a)

instance Monoidal (->) where
  f `x` g = \(a, b) -> (f a, g b)

instance Cartesian (->) where
  exl = \(a, _) -> a
  exr = \(_, b) -> b
  dup = \a -> (a, a)

-------------------------------------

instance {-# OVERLAPS #-} (Num a) => Additive a where
  zero = 0
  one = 1
  (^+) = (+)

instance {-# OVERLAPS #-} (Additive a, Additive b) => Additive (a, b) where
  zero = (zero, zero)
  one = (one, one)
  (a1, b1) ^+ (a2, b2) = (a1 ^+ a2, b1 ^+ b2)

-------------------------------------

-- This isn't really used anymore?

inlF :: Additive b => a -> (a, b)
inrF :: Additive a => b -> (a, b)
jamF :: Additive a => (a, a) -> a

inlF = \a -> (a, zero)
inrF = \b -> (zero, b)
jamF = \(a, b) -> a ^+ b

