{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util
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

newtype D k a b = D {eval :: a -> (b, a `k` b)}

linearD :: (a -> b) -> (a `k` b) -> D k a b
linearD f f' = D $ \a -> (f a, f')

instance Cat.Category k => Cat.Category (D k) where
  id      = linearD id Cat.id
  g . f   = D $ \a -> let (b, f') = eval f a
                          (c, g') = eval g b
                      in (c, g' Cat.. f')

instance Cat.Monoidal k => Cat.Monoidal (D k) where
  f `x` g = D $ \(a, b) -> let (c, f') = eval f a
                               (d, g') = eval g b
                           in ((c, d), f' `Cat.x` g')

instance Cat.Cartesian k => Cat.Cartesian (D k) where
  exl = linearD Cat.exl Cat.exl
  exr = linearD Cat.exr Cat.exr
  dup = linearD Cat.dup Cat.dup

t :: Cat.Cartesian k => (a `k` c) -> (a `k` d) -> (a `k` (c, d))
f `t` g = (f `Cat.x` g) Cat.. Cat.dup

v :: (Num a, 
      Cat.Monoidal k, 
      Cat.Cocartesian k) => (c `k` a) -> (d `k` a) -> ((c, d) `k` a)
f `v` g = Cat.jam Cat.. (f `Cat.x` g)

newtype a ->+ b = AddFun (a -> b)

instance Cat.Category (->+) where
  id = AddFun id
  (AddFun g) . (AddFun f) = AddFun (g . f)

instance Cat.Monoidal (->+) where
  (AddFun f) `x` (AddFun g) = AddFun (f `Cat.x` g)

instance Cat.Cartesian (->+) where
  exl = AddFun Cat.exl
  exr = AddFun Cat.exr
  dup = AddFun Cat.dup

instance Cat.Cocartesian (->+) where
  inl = AddFun $ \a -> (a, 0)
  inr = AddFun $ \b -> (0, b)
  jam = AddFun $ \(a, b) -> a + b

instance Num a => Cat.Scalable (->+) a where
  scale a = AddFun $ \x -> a*x

----------------
 
instance Num a => Cat.NumCat (->) a where
  negateC = negate
  addC = uncurry (+)
  mulC = uncurry (*)

-- There seems to be too many constraints here, but it doesn't work otherwise?
instance (Cat.Monoidal k,
          Cat.Cocartesian k, 
          Cat.Scalable k s, 
          Cat.NumCat k s, 
          Num s) => Cat.NumCat (D k) s where
  negateC = linearD Cat.negateC Cat.negateC
  addC = linearD Cat.addC Cat.addC
  mulC = D $ \(a, b) -> (a * b, (Cat.scale b) `v` (Cat.scale a))

{-
f :: (Cat.NumCat k a,
      Cat.Cartesian k) => k a a
f = Cat.mulC Cat.. (Cat.id `t` Cat.id)


-}

newtype Cont k r a b = Cont ((b `k` r) -> (a `k` r))

cont :: Cat.Category k => (a `k` b) -> Cont k r a b
cont f = Cont (Cat.. f)

apply :: Cat.Category k => Cont k r a b -> (b `k` r) -> (a `k` r)
apply (Cont f) g = f g

instance Cat.Category k => Cat.Category (Cont k r) where
  id = Cont id
  (Cont g) . (Cont f) = Cont (f . g)

-------------------

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
