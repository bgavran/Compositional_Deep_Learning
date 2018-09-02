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
             UndecidableInstances,
             GeneralizedNewtypeDeriving
                            #-}

module CategoricDefinitions where

import Prelude hiding (id, (.))
import qualified Prelude as P
import GHC.Exts (Constraint)


class Category k where
  type Allowed k a :: Constraint
  type Allowed k a = ()

  id  :: Allowed k a => a `k` a
  (.) :: Allowed3 k a b c => (b `k` c) -> (a `k` b) -> (a `k` c)

-- By monoidal here we mean symmetric monoidal category
class Category k => Monoidal k where
  -- unit object in haskell is ()
  x :: Allowed6 k a b c d (a, b) (c, d)
    => (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d))
  assocL :: Allowed7 k a b c (a, b) ((a, b), c) (b, c) (a, (b, c))
         => ((a, b), c) `k` (a, (b, c))
  assocR :: Allowed7 k a b c (a, b) ((a, b), c) (b, c) (a, (b, c))
         => (a, (b, c)) `k` ((a, b), c)
  --unitorL :: Allowed k a
  --        => ((), a) `k` a
  --unitorL' :: Allowed k a -- inverse of unitorL
  --         => a `k` ((), a)
  --unitorR :: Allowed k a
  --        => (a, ()) `k` a
  --unitorR' :: Allowed k a -- inverse of unitorR
  --         => a `k` (a, ())
  swap :: Allowed4 k a b (a, b) (b, a)
       => (a, b) `k` (b, a)

class Monoidal k => Cartesian k where
  type AllowedCar k a :: Constraint
  type AllowedCar k a = ()

  exl :: AllowedCar k b => (a, b) `k` a
  exr :: AllowedCar k a => (a, b) `k` b
  dup :: AllowedCar k a => a `k` (a, a)
  counit :: AllowedCar k a => a `k` ()

class Category k => Cocartesian k where
  type AllowedCoCar k a :: Constraint
  type AllowedCoCar k a = Allowed k a

  inl :: AllowedCoCar k b => a `k` (a, b)
  inr :: AllowedCoCar k a => b `k` (a, b)
  jam :: AllowedCoCar k a => (a, a) `k` a
  unit :: AllowedCoCar k a => () `k` a

--------------------------------------


class NumCat k a where
  negateC :: a `k` a
  addC :: (a, a) `k` a
  mulC :: (a, a) `k` a

class FloatCat k a where
  expC :: a `k` a

class Scalable k a where
  scale :: a -> (a `k` a)

class Additive a where
  zero :: a
  (^+) :: a -> a -> a

-------------------------------------
-- Instances
-------------------------------------

instance Category (->) where
  id    = \a -> a
  g . f = \a -> g (f a)

instance Monoidal (->) where
  f `x` g = \(a, b) -> (f a, g b)
  assocL = \((a, b), c) -> (a, (b, c))
  assocR = \(a, (b, c)) -> ((a, b), c)
  --unitorL = \((), a) -> a
  --unitorL' = \a -> ((), a)
  --unitorR = \(a, ()) -> a
  --unitorR' = \a -> (a, ())
  swap = \(a, b) -> (b, a)

instance Cartesian (->) where
  exl = \(a, _) -> a
  exr = \(_, b) -> b
  dup = \a -> (a, a)
  counit = \_ -> ()

instance Num a => NumCat (->) a where
  negateC = negate
  addC = uncurry (+)
  mulC = uncurry (*)

instance Floating a => FloatCat (->) a where
  expC = exp


-------------------------------------

--instance {-# OVERLAPS #-} Additive () where
--  zero = ()
--  () ^+ () = ()

instance {-# OVERLAPS #-} (Num a) => Additive a where
  zero = 0
  (^+) = (+)

instance {-# OVERLAPS #-} (Additive a, Additive b) => Additive (a, b) where
  zero = (zero, zero)
  (a1, b1) ^+ (a2, b2) = (a1 ^+ a2, b1 ^+ b2)


-------------------------------------

type Allowed2 k a b = (Allowed k a, Allowed k b)
type Allowed3 k a b c = (Allowed2 k a b, Allowed k c)
type Allowed4 k a b c d = (Allowed3 k a b c, Allowed k d)
type Allowed5 k a b c d e = (Allowed4 k a b c d, Allowed k e)
type Allowed6 k a b c d e f = (Allowed5 k a b c d e, Allowed k f)
type Allowed7 k a b c d e f g = (Allowed6 k a b c d e f, Allowed k g)
type Allowed8 k a b c d e f g h = (Allowed7 k a b c d e f g, Allowed k h)
type Allowed9 k a b c d e f g h i = (Allowed8 k a b c d e f g i, Allowed k i)

--(/\) :: (Cartesian k, Allowed k b, Allowed k (b, b), Allowed k (c, d))
--     => b `k` c -> b `k` d -> b `k` (c, d)
f /\ g = (f `x` g) . dup

--(\/) :: (Cocartesian k, Monoidal k, Allowed k (a, b), Allowed k (c, c), Allowed k c)
--     => a `k` c -> b `k` c -> (a, b) `k` c
f \/ g = jam . (f `x` g)

--fork :: (Cartesian k, Allowed k b, Allowed k (b, b), Allowed k (c, d))
--     => (b `k` c,  b `k` d) -> b `k` (c, d)
fork (f, g) = f /\ g

--unfork :: (Cartesian k, Allowed k b, Allowed k (c, d), Allowed k c, Allowed k d)
--       => b `k` (c, d) -> (b `k` c, b `k` d)
unfork h = (exl . h, exr . h)

--join :: (Cocartesian k, Monoidal k, Allowed k (a, b), Allowed k (c, c), Allowed k c) 
--     => (a `k` c, b `k` c) -> (a, b) `k` c
join (f, g) = f \/ g

--unjoin :: (Cocartesian k, Monoidal k, Allowed k a, Allowed k (a, b), Allowed k c, Allowed k b)
--       => (a, b) `k` c -> (a `k` c, b `k` c) 
unjoin h = (h . inl, h . inr)
