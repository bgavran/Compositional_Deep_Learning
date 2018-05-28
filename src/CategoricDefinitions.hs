{-# LANGUAGE 
             EmptyCase,
             FlexibleInstances,
             FlexibleContexts,
             InstanceSigs,
             MultiParamTypeClasses,
             PartialTypeSignatures,
             LambdaCase,
             MultiWayIf,
             NamedFieldPuns,
             TupleSections,
             DeriveFunctor,
             TypeOperators,
             ScopedTypeVariables,
             ConstraintKinds,
             RankNTypes,
             NoMonomorphismRestriction,
             TypeFamilies,
             UndecidableInstances 
                            #-}

module CategoricDefinitions where

import GHC.Exts (Constraint)


class NoConstraint a where

instance NoConstraint a where

class Category k where
  type Allowed k a :: Constraint
  type Allowed k a = ()

  id  :: Allowed k a => a `k` a
  (.) :: Allowed3 k a b c => (b `k` c) -> (a `k` b) -> (a `k` c)

 --x :: (Allowed k a,
 --      Allowed k b,
 --      Allowed k c,
 --      Allowed k d,
 --      Allowed k (a, b),
 --      Allowed k (c, d)) => (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d)) 
 --x :: (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d)) 
class Category k => Monoidal k where
--  x :: (Allowed k a,
--        Allowed k b,
--        Allowed k c,
--        Allowed k d,
--        Allowed k (a, b),
--        Allowed k (c, d)) => (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d)) 
  x :: (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d)) 


class Monoidal k => Cartesian k where
  exl :: (a, b) `k` a
  exr :: (a, b) `k` b
  dup :: a `k` (a, a)

class Category k => Cocartesian k where
  inl :: Allowed k b => a `k` (a, b)
  inr :: Allowed k a => b `k` (a, b)
  jam :: Allowed k a => (a, a) `k` a

class Cartesian k => Closed k e where
  apply :: (a `e` b, a) `k` b
  curry :: ((a, b) `k` c) -> a `k` (b `e` c)
  uncurry :: a `k` (b `e` c) -> (a, b) `k` c

type Allowed3 k a b c = (Allowed k a, 
                         Allowed k b, 
                         Allowed k c)

type AllowedSpecial1 k a b c = (Allowed k (a, b), Allowed k c, Allowed k (c, c))
type AllowedSpecial2 k a b c = (Allowed3 k a b c, Allowed k (a, b))

(^.) = (CategoricDefinitions..)

(/\) :: (Cartesian k, AllowedSpecial1 k c d b) => b `k` c -> b `k` d -> b `k` (c, d)
f /\ g = (f `x` g) ^. dup 

(\/) :: (Cocartesian k, Monoidal k, AllowedSpecial1 k a b c) => a `k` c -> b `k` c -> (a, b) `k` c
f \/ g = jam ^. (f `x` g)


-- Uncurried versions of the above ops and their inverses
fork :: (Cartesian k, AllowedSpecial1 k c d b) => (b `k` c, b `k` d) -> b `k` (c, d)
fork (f, g) = f /\ g

unfork :: (Cartesian k, AllowedSpecial2 k c d b) => b `k` (c, d) -> (b `k` c, b `k` d)
unfork h = (exl ^. h, exr ^. h)

join :: (Cocartesian k, Monoidal k, AllowedSpecial1 k a b c) => (a `k` c, b `k` c) -> (a, b) `k` c
join (f, g) = f \/ g

unjoin :: (Cocartesian k, Monoidal k, AllowedSpecial2 k a b c) => (a, b) `k` c -> (a `k` c, b `k` c) 
unjoin h = (h ^. inl, h ^. inr)

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

instance Category (->) where
  id    = \a -> a
  g . f = \a -> g (f a)

instance Monoidal (->) where
  f `x` g = \(a, b) -> (f a, g b)

instance Cartesian (->) where
  exl = \(a, _) -> a
  exr = \(_, b) -> b
  dup = \a -> (a, a)

instance Closed (->) (->) where
  apply (f, a) = f a
  curry f      = \a b -> f (a, b)
  uncurry f    = \(a, b) -> f a b

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

