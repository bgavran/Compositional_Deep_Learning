{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

import qualified CategoricDefinitions as Cat

----------------

instance Cat.Category (->) where
  id    = \a -> a
  g . f = \a -> g (f a)

instance Cat.Monoidal (->) where
  f `x` g = \(a, b) -> (f a, g b)

instance Cat.Cartesian (->) where
  exl = \(a, _) -> a
  exr = \(_, b) -> b
  dup = \a -> (a, a)

----------------

newtype GD k a b = D {eval :: a -> (b, a `k` b)}

type (~>) = GD (->)

linearD :: (a -> b) -> (a `k` b) -> GD k a b
linearD f f' = D $ \a -> (f a, f')

instance Cat.Category k => Cat.Category (GD k) where
  type Allowed (GD k) = Cat.Allowed k
  id      = linearD Cat.id Cat.id
  (D g) . (D f)   = D $ \a -> let (b, f') = f a
                                  (c, g') = g b
                              in (c, g' Cat.. f')

instance Cat.Monoidal k => Cat.Monoidal (GD k) where
  (D f) `x` (D g) = D $ \(a, b) -> let (c, f') = f a
                                       (d, g') = g b
                                   in ((c, d), f' `Cat.x` g')

instance Cat.Cartesian k => Cat.Cartesian (GD k) where
  exl = linearD Cat.exl Cat.exl
  exr = linearD Cat.exr Cat.exr
  dup = linearD Cat.dup Cat.dup

-- Num constraint needs to exist for class (GD k) for this to work
--instance Cat.Cocartesian k => Cat.Cocartesian (GD k) where
--  inl = linearD inlF Cat.inl
--  inr = linearD inrF Cat.inr
--  jam = linearD jamF Cat.jam

type TriangleAllowed k a c d = (Cat.Allowed k a, 
                                Cat.Allowed k c,
                                Cat.Allowed k d,
                                Cat.Allowed k (a, a),
                                Cat.Allowed k (c, d))

(/\) :: (TriangleAllowed k a c d, 
         Cat.Cartesian k) => (a `k` c) -> (a `k` d) -> (a `k` (c, d))
f /\ g = (f `Cat.x` g) Cat.. Cat.dup

(\/) :: (TriangleAllowed k a c d,        
         Cat.Monoidal k, 
         Cat.Cocartesian k) => (c `k` a) -> (d `k` a) -> ((c, d) `k` a)
f \/ g = Cat.jam Cat.. (f `Cat.x` g)

---

newtype a ->+ b = AddFun (a -> b)

instance Cat.Category (->+) where
  type Allowed (->+) = Num
  id = AddFun id
  (AddFun g) . (AddFun f) = AddFun (g . f)

instance Cat.Monoidal (->+) where
  (AddFun f) `x` (AddFun g) = AddFun (f `Cat.x` g)

instance Cat.Cartesian (->+) where
  exl = AddFun Cat.exl
  exr = AddFun Cat.exr
  dup = AddFun Cat.dup

instance Cat.Cocartesian (->+) where
  inl = AddFun inlF
  inr = AddFun inrF
  jam = AddFun jamF

inlF :: Num b => a -> (a, b)
inrF :: Num a => b -> (a, b)
jamF :: Num a => (a, a) -> a

inlF = \a -> (a, 0)
inrF = \b -> (0, b)
jamF = \(a, b) -> a + b

instance Num a => Cat.Scalable (->+) a where
  scale a = AddFun (*a)

----------------
 
instance Num a => Cat.NumCat (->) a where
  negateC = negate
  addC = uncurry (+)
  mulC = uncurry (*)

-- There seems to be too many constraints here, but it doesn't work otherwise?
instance (Cat.Monoidal k,
          Cat.Cocartesian k, 
          Cat.Scalable k a, 
          Cat.NumCat k a, 
          Cat.Allowed k a,
          Cat.Allowed k (a, a),
          Num a) => Cat.NumCat (GD k) a where
  negateC = linearD Cat.negateC Cat.negateC
  addC = linearD Cat.addC Cat.addC
  mulC = D $ \(a, b) -> (a * b, Cat.scale b \/ Cat.scale a)


f :: (Cat.NumCat k a,
      Cat.Allowed k a,
      Cat.Allowed k (a, a),
      Cat.Cartesian k) => k a a
f = Cat.mulC Cat.. (Cat.id /\ Cat.id)



newtype Cont k r a b = Cont ((b `k` r) -> (a `k` r))

cont :: (Cat.Allowed k a,
         Cat.Allowed k b,
         Cat.Allowed k r, 
         Cat.Category k) => (a `k` b) -> Cont k r a b
cont f = Cont (Cat.. f)

apply :: Cat.Category k => Cont k r a b -> (b `k` r) -> (a `k` r)
apply (Cont f) g = f g

instance Cat.Category k => Cat.Category (Cont k r) where
  id = Cont id
  (Cont g) . (Cont f) = Cont (f . g)
