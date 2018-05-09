{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util
import qualified CategoricDefinitions as Cat

----------------

instance Num a => Cat.NumCat (->) a where
  negateC = negate
  addC = uncurry (+)
  mulC = uncurry (*)

----------------

newtype GD k a b = D {eval :: a -> (b, a `k` b)}

type (~>) = GD (->)

linearD :: (a -> b) -> (a `k` b) -> GD k a b
linearD f f' = D $ \a -> (f a, f')

instance Cat.Category k => Cat.Category (GD k) where
  type Allowed (GD k) a = Cat.Allowed k a
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

-- There seems to be too many constraints here, but it doesn't work otherwise?
--instance (Cat.Monoidal k,
--          Cat.Cocartesian k, 
--          Cat.Scalable k a, 
--          Cat.NumCat k a, 
--          Cat.Allowed k a,
--          Cat.Allowed k (a, a),
--          Num a) => Cat.NumCat (GD k) a where
--  negateC = linearD Cat.negateC Cat.negateC
--  addC = linearD Cat.addC Cat.addC
--  mulC = D $ \(a, b) -> (a * b, Cat.scale b \/ Cat.scale a)



type TriangleAllowed k a c d = (Cat.Allowed k a, 
                                Cat.Allowed k c,
                                Cat.Allowed k d,
                                Cat.Allowed k (a, a),
                                Cat.Allowed k (c, d))

(\/) :: (TriangleAllowed k a c d,        
         Cat.Monoidal k, 
         Cat.Cocartesian k) => (c `k` a) -> (d `k` a) -> ((c, d) `k` a)
f \/ g = Cat.jam Cat.. (f `Cat.x` g)

(/\) :: (TriangleAllowed k a c d, 
         Cat.Cartesian k) => (a `k` c) -> (a `k` d) -> (a `k` (c, d))
f /\ g = (f `Cat.x` g) Cat.. Cat.dup



(#*) :: NArray None Double -> NArray None Double -> NArray None Double
(#*) = (*)

(#+) :: NArray None Double -> NArray None Double -> NArray None Double
(#+) = (+)

oMul = D $ \(a, b) -> (a #* b, \(da, db) -> da #* b #+ db #* a) 

oId  = D $ \a -> (a, id)

d = oId /\ oId

deval = eval d

f1 = oMul Cat.. d

fn = eval f1

t = fn 3
v = fst t

g = snd t

--------------------

ds # cs = listArray ds cs :: Array Double

sh x = putStr . formatFixed 2 $ x

a = [2, 3] # [1..] ! "ij"
b = [3, 5] # [10..] ! "jk"

c = a * b

-- Assumes single-letter index names
onesLike c = let d  = map iDim $ dims c
                 ch = concat $ map iName $ dims c
             in d # (repeat 1) ! ch

aGrad = (onesLike c) * b
bGrad = (onesLike c) * a

---------------



---

newtype a ->+ b = AddFun (a -> b)

instance Cat.Category (->+) where
  type Allowed (->+) a = Num a -- Should be Additive
  id = AddFun id
  (AddFun g) . (AddFun f) = AddFun (g . f)

instance Cat.Monoidal (->+) where
  (AddFun f) `x` (AddFun g) = AddFun (f `Cat.x` g)

instance Cat.Cartesian (->+) where
  exl = AddFun Cat.exl
  exr = AddFun Cat.exr
  dup = AddFun Cat.dup

instance Cat.Cocartesian (->+) where
  inl = AddFun Cat.inlF
  inr = AddFun Cat.inrF
  jam = AddFun Cat.jamF

instance Num a => Cat.NumCat (->+) a where
  negateC = AddFun Cat.negateC
  addC = AddFun Cat.addC
  mulC = AddFun Cat.mulC

instance Num a => Cat.Scalable (->+) a where
  scale a = AddFun (*a)

----------------










-- Continuations

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
