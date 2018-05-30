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

import Prelude hiding ((.))
import qualified Prelude as P
import GHC.Exts (Constraint)


class NoConstraint a where

instance NoConstraint a where

class Category k where
  type Allowed k a :: Constraint
  type Allowed k a = ()

  type AllowedComp k a b c :: Constraint
  type AllowedComp k a b c = (Allowed k a, Allowed k b, Allowed k c)

  id  :: Allowed k a => a `k` a
  (.) :: AllowedComp k a b c => (b `k` c) -> (a `k` b) -> (a `k` c)

 --x :: (Allowed k a,
 --      Allowed k b,
 --      Allowed k c,
 --      Allowed k d,
 --      Allowed k (a, b),
 --      Allowed k (c, d)) => (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d)) 
class Category k => Monoidal k where
  x :: (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d)) 


class Monoidal k => Cartesian k where
  exl :: (a, b) `k` a
  exr :: (a, b) `k` b
  dup :: a `k` (a, a)

class Category k => Cocartesian k where
  inl :: Allowed k b => a `k` (a, b)
  inr :: Allowed k a => b `k` (a, b)
  jam :: Allowed k a => (a, a) `k` a

class Cartesian k => Closed k where
  apply :: (a `k` b, a) `k` b
  curry :: AllowedComp k a b c => ((a, b) `k` c) -> a `k` (b `k` c)
  uncurry :: a `k` (b `k` c) -> (a, b) `k` c

(/\) :: (Cartesian k, AllowedComp k b (b, b) (c, d)) => b `k` c -> b `k` d -> b `k` (c, d)
f /\ g = (f `x` g) . dup 

(\/) :: (Cocartesian k, Monoidal k, AllowedComp k (a, b) (c, c) c, Allowed k c) => a `k` c -> b `k` c -> (a, b) `k` c
f \/ g = jam . (f `x` g)


---- Uncurried versions of the above ops and their inverses
fork :: (Cartesian k, AllowedComp k b (b, b) (c, d)) 
     => (b `k` c, b `k` d) -> b `k` (c, d)
fork (f, g) = f /\ g

unfork :: (Cartesian k, AllowedComp k b (c, d) c, AllowedComp k b (c, d) d) 
       => b `k` (c, d) -> (b `k` c, b `k` d)
unfork h = (exl . h, exr . h)

join :: (Cocartesian k, Monoidal k, AllowedComp k (a, b) (c, c) c, Allowed k c) 
     => (a `k` c, b `k` c) -> (a, b) `k` c
join (f, g) = f \/ g

unjoin :: (Cocartesian k, Monoidal k, AllowedComp k a (a, b) c, AllowedComp k b (a, b) c, Allowed k a, Allowed k b) 
       => (a, b) `k` c -> (a `k` c, b `k` c) 
unjoin h = (h . inl, h . inr)

--------------------------------------

class Additive a where
  zero :: a
  one :: a
  (^+) :: a -> a -> a

type Additive2 a b = (Additive a, Additive b)
type Additive3 a b c = (Additive2 a b, Additive c)
type Additive4 a b c d = (Additive3 a b c, Additive d)
type Additive5 a b c d e = (Additive4 a b c d, Additive e)
type Additive6 a b c d e f = (Additive5 a b c d e, Additive f)
type Additive7 a b c d e f g = (Additive6 a b c d e f, Additive g)
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

instance Closed (->) where
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
