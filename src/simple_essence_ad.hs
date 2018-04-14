{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Control.Category as Cat

newtype Fn a b = Fn {eval :: a -> (b, a -> b)}

linearF :: (a -> b) -> Fn a b
linearF f = Fn $ \a -> (f a, f)

instance Cat.Category Fn where
  id      = linearF id
  g . f   = Fn $ \a -> let (b, f') = eval f a
                           (c, g') = eval g b
                       in (c, g' . f')

class Cat.Category k => Monoidal k where
  x :: (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d)) 

instance Monoidal Fn where
  f `x` g = Fn $ \(a, b) -> let (c, f') = eval f a
                                (d, g') = eval g b
                            in ((c, d), \(z1, z2) -> (f' z1, g' z2))

class Monoidal k => Cartesian k where
  exl :: (a, b) `k` a
  exr :: (a, b) `k` b
  dup :: a `k` (a, a)

instance Cartesian Fn where
  exl = linearF fst
  exr = linearF snd
  dup = linearF $ \x -> (x, x)

-- Error in the paper, in paper cocartesian requires just category.
-- Or is it a mistake, perhaps the 'x' is the 'x' I defined already and not tuple?

class Monoidal k => Cocartesian k where
  inl :: a `k` (a, b)
  inr :: b `k` (a, b)
  jam :: (a, a) `k` a

t :: Cartesian k => (a `k` c) -> (a `k` d) -> (a `k` (c, d))
f `t` g = (f `x` g) Cat.. dup

v :: Cocartesian k => (c `k` a) -> (d `k` a) -> ((c, d) `k` a)
f `v` g = jam Cat.. (f `x` g)

class NumCat k a where
  negateC :: a `k` a
  addC :: (a, a) `k` a
  mulC :: (a, a) `k` a
 
instance Num a => NumCat (->) a where
  negateC = negate
  addC = uncurry (+)
  mulC = uncurry (*)

