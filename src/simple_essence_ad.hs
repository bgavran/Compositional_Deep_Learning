{-# LANGUAGE TypeOperators #-}

import qualified Control.Category as Cat

newtype Fn a b = Fn {eval :: a -> (b, b -> a)}

--linearF :: (a -> b) -> Fn a b
--linearF f = Fn $ \a -> (f a, f)

instance Cat.Category Fn where
  id      = Fn $ \x -> (x, id)
  g . f   = Fn $ \a -> let (b, f') = eval f a
                           (c, g') = eval g b
                       in (c, f' . g')

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

--instance Cartesian Fn where
--  exl = Fn $ \(x, y) -> (x, \x -> (x, 0))
--  dup = Fn $ \x -> ((x, x), \(x0, x1) -> x0 + x1)


sigm :: Fn Double Double
sigm = Fn $ \x -> let y = 1 / (1 + (exp $ negate x))
                  in (y, \z -> z * y * (1 - y))

mul :: Fn (Double, Double) Double
mul  = Fn $ \(x, y) -> (x * y, \z -> (z * y, z * x))

comul :: Fn Double (Double, Double)
comul = Fn $ \x -> ((x, x), \(z1, z2) -> z1 + z2)

myFst :: Num c => Fn a (b, c) -> Fn a b
myFst n = Fn $ \a -> let ((b, c), dBCdA) = eval n a
                     in (b, \z -> dBCdA (z, 0))

mySnd :: Num b => Fn a (b, c) -> Fn a c
mySnd n = Fn $ \a -> let ((b, c), dBCdA) = eval n a
                     in (c, \z -> dBCdA (0, z))

{-


OUTPUT ==>  mul
	   /   \
	  /     \
	sigm     |
	   \    /
	    \  /
           (x, x)
             |
           comul
             |
             |
INPUT ==>    x 
-}

left = myFst comul
right = mySnd comul

s = sigm Cat.. left

tpl = s `x` right

m = mul Cat.. tpl

--





main = putStrLn "alpha" >>= k

k = \_ -> putStrLn "beta" >>= j

j = \_ -> putStrLn "gamma" 
