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

newtype D a b = D {eval :: a -> (b, b -> a)}

instance Cat.Category D where
  type Allowed D a = Cat.Additive a
  id      = D $ \a -> (a, Cat.id)
  (D g) . (D f)   = D $ \a -> let (b, f') = f a
                                  (c, g') = g b
                              in (c, f' Cat.. g')

instance Cat.Monoidal D where
  (D f) `x` (D g) = D $ \(a, b) -> let (c, f') = f a
                                       (d, g') = g b
                                   in ((c, d), f' `Cat.x` g')

instance Cat.Cartesian D where
  exl = D $ \(a, _) -> (a, Cat.inlF)
  exr = D $ \(_, b) -> (b, Cat.inrF)
  dup = D $ \a -> ((a, a), Cat.jamF)


--add :: D (Double, Double) Double
add = D $ \(a, b) -> ((a + b), \dm -> (dm, dm))

--mul :: D (Double, Double) Double
mul = D $ \(a, b) -> ((a * b), \dm -> (dm * b, dm * a))


--sigm :: D Double Double
sigm = let s x = 1 / (1 + exp (-x))
       in D $ \a -> (s a, \dm -> dm * (s a) * (1 - s a))

add5 = D $ \a -> ((a + 5 :: Double), id)

fn = add Cat.. (add5 `Cat.x` (sigm Cat.. mul))
emul = eval fn

v = emul (2, (0.1, 0.5 :: Double))

f = fst v
df = snd v

infixl 8 −|
(−|) :: Name -> [Array Double] -> Array Double
(−|) = index

mean name t = let n = size name t
                  v = scalar . recip . fromIntegral $ n
               in t * name −| (replicate n v) where



--- Tensor manipulations

ds # cs = listArray ds cs :: Array Double

sh :: Compat i => NArray i Double -> IO ()
sh x = putStr . formatFixed 2 $ x

p1 = [2, 3] # [0.1, 0.1..] ! "ij"
p2 = [3, 5] # [0.2, 0.2..] ! "jk"

p3 = p1 * (p2 ! "kl")

-- Need to make this differentiable?
sumAxes :: NArray None Double -> [Char] -> NArray None Double
sumAxes arr names = let s = map (\ind -> size [ind] arr) names
                        o = s # [1..] ! names
                    in arr * o

--newtype MyCont r a b = Cont ( (b -> r) -> (a -> r))
--
--cont :: (a -> b) -> MyCont r a b
--cont f = Cont (. f)
--
--apply :: MyCont r a b -> (b -> r) -> a -> r
--apply (Cont f) g = f g
--
--newtype MyDual a b = Dual (b -> a)
