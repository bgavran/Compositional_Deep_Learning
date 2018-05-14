{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util
import qualified CategoricDefinitions as Cat
import Control.Monad

--------------------------------

data Z p
  = NoP
  | P p
  | Z p `X` Z p
  deriving (Eq, Show, Functor)

instance Applicative Z where
  pure = P
  NoP       <*> p = NoP
  (P f)     <*> p = fmap f p
  (f `X` g) <*> p = (f <*> p) `X` (g <*> p)
  
--------------------------------

newtype D a b = D {
  eval :: a -> (b, D b a) -- D b a is here instead of b -> a because sometimes we'd like to have higher order gradients
}

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
  exl = D $ \(a, _) -> (a, Cat.inl)
  exr = D $ \(_, b) -> (b, Cat.inr)
  dup = D $ \a -> ((a, a), Cat.jam)

instance Cat.Cocartesian D where
  inl = D $ \a -> ((a, Cat.zero), Cat.exl)
  inr = D $ \b -> ((Cat.zero, b), Cat.exr)
  jam = D $ \(a, b) -> (a Cat.^+ b, Cat.dup)

--------------------------------

newtype Para p a b = Para {
  evalPara :: D (Z p, a) b
}

-- Parametrized function D (Z p, a) b is fundamentally different than just D a b?
--eval :: Z p -> a -> a -> (b, D b (Z p -> a -> a)) -- perhaps currying can be used here to make the code just below more elegant?

instance Cat.Category (Para p) where
  id = let f = D $ \(_, a) -> (a, D $ \b' -> ((NoP, b'), f))
       in Para f
  (Para dg) . (Para df) = let h = D $ \(p `X` q, a) -> let (b, f') = eval df (p, a)
                                                           (c, g') = eval dg (q, b)
                                                       in (c, D $ \c' -> let ((q', b'), g'') = eval g' c'
                                                                             ((p', a'), f'') = eval f' b'
                                                                         in ((p' `X` q', a'), evalPara $ (Para g'') Cat.. (Para f'')))
                          in Para h

instance Cat.Monoidal (Para p) where
  (Para df) `x` (Para dg) = let h = D $ \(p `X` q, (a, b)) -> let (c, f') = eval df (p, a)
                                                                  (d, g') = eval dg (q, b)
                                                              in ((c, d), D $ \(c', d') -> let ((p', a'), f'') = eval f' c'
                                                                                               ((q', b'), g'') = eval g' d'
                                                                                           in ((p' `X` q', (a', b')), evalPara $ (Para f'') `Cat.x` (Para g'')))
                            in Para h

--------------------------------

type ZF p = Z p
type ImplReqF p a b = Para p a b
type UpdF p = (Z p, Z p) -> Z p
type CostF b = (b, b) -> (b, (b, b)) -- Usually would have been instantiated as D (b, b) b, but Haskell complains about multiple instances declaration... :/

data Learner p a b = L {
  param :: ZF p,
  implreq  :: ImplReqF p a b,
  upd   :: UpdF p,
  cost  :: CostF b
}

instance Cat.Category (Learner p) where
  id = L {param = NoP,
          implreq = Cat.id,
          upd    = \_ -> NoP,
          cost   = trivialCost}

  L p2 ir2' u2 c2 . L p1 ir1' u1 c1 = let ir1 = evalPara ir1'
                                          ir2 = evalPara ir2'
                                      in L {param = p1 `X` p2,
                                            implreq  = let h = D $ \(p `X` q, a) -> let (b, f')  = eval ir1 (p, a)
                                                                                        (c, g')  = eval ir2 (q, b)
                                                                                        costGrad = \b' -> (snd . snd) $ c1 (b, b')
                                                                                    in (c, D $ \c' -> let ((q', b'), g'') = eval g' c'
                                                                                                          ((p', a'), f'') = ((eval f') . costGrad) b'
                                                                                                      in ((p' `X` q', a'), evalPara $ (Para g'') Cat.. (Para f'')))
                                                       in Para h,
                                            upd   = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
                                            cost   = c2} 

-- Is cost function a part of a learner? Cost function fits somewhere in between learners, when composing them? as part of learner composition?

instance Cat.Monoidal (Learner p) where
  L p1 ir1 u1 c1 `x` L p2 ir2 u2 c2 = L {param = p1 `X` p2,
                                         implreq = ir1 `Cat.x` ir2,
                                         upd = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
                                         cost = \((b, c), (b', c')) -> let (b, (bTrue', bPred')) = c1 (b, b')
                                                                           (c, (cTrue', cPred')) = c2 (c, c')
                                                                       in ((b, c), ((bTrue', cTrue'), (bPred', cPred')))}

--------------------------------

sgd :: (Num p, Fractional p) => p -> p -> p
sgd p pGrad = p - 0.001 * pGrad


sqrError :: Num a => (a, a) -> (a, (a, a)) 
sqrError = \(c, c') -> ((c - c')^2, let v = 2 * (c - c')
                                    in (v, -v))

trivialCost :: CostF b
trivialCost = \(b, b') -> (undefined, (b, b'))

add :: Cat.Additive a => D (a, a) a
add = Cat.jam

zadd :: Num a => D (Z a, a) a
zadd = let m = D $ \(P a, b) -> (a + b, D $ \dm -> ((P dm, dm), m))
       in m

mul :: Num a => D (a, a) a
mul = let m = D $ \(a, b) -> (a * b, D $ \dm -> ((dm * b, dm * a), m))
      in m

zmul :: Num a => D (Z a, a) a
zmul = let m = D $ \(P a, b) -> (a * b, D $ \dm -> ((P $ dm * b, dm * a), m))
       in m

sigm :: Floating a => D a a
sigm = let sFn x = 1 / (1 + exp (-x))
           s = D $ \a -> (sFn a, D $ \dm -> (dm * (sFn a) * (1 - sFn a), s))
       in s

paraFnMul :: Para Double Double Double
paraFnMul = Para zmul

paraFnAdd :: Para Double Double Double
paraFnAdd = Para zadd

-- This is basically a functor from Para -> Learn. Except we need to fix cost and update functions.
functorL :: Para p a b -> (p -> p -> p) -> CostF b -> Learner p a b
functorL para u c = L {
  param = undefined, -- initialized randomly in the shape of param?
  implreq = para,
  upd = \(p, pGrad) -> u <$> p <*> pGrad, -- just applying the update fn recursively to the param data stucture
  cost = c
}

l1 = functorL paraFnMul sgd trivialCost

l2 = functorL paraFnAdd sgd sqrError

l3 = l2 Cat.. l1

l4 = l2 `Cat.x` l1

fn d v = fst $ eval d v

--ir :: Learner p a b -> a -> (b, b -> (Z p, a))
--ir l a = (eval $ implreq l) (param l, a)
--
--f :: Learner p a b -> a -> b
--f l a = fst $ ir l a
--
--df :: Learner p a b -> a -> (b -> (Z p, a))
--df l a = snd $ ir l a
--
--learnerGrad :: Learner Double Double Double -> Double -> (Z Double, Double)
--learnerGrad l = df l 10
--
--updateLearner :: Learner Double Double Double -> Learner Double Double Double
--updateLearner l = let pGrad = fst $ (learnerGrad l) 1 
--                  in l {param = (upd l) (param l, pGrad) }
--
-- Tensor manipulations

ds # cs = listArray ds cs :: Array Double

sh x = putStr . formatFixed 2 $ x

p1 = [2, 3] # [0.1, 0.1..] ! "ij"
p2 = [3, 5] # [0.01, 0.01..] ! "jk"

c = p1 * p2

-- Assumes single-letter index names
onesLike c = let d  = map iDim $ dims c
                 ch = concat $ map iName $ dims c
             in d # (repeat 1) ! ch

p1Grad = (onesLike c) * p2
p2Grad = (onesLike c) * p1
--
----lT1 = L {
----  param = P p1,
----  implreq = \(P p) a -> ((a * p), (*p)),
----  upd = undefined,
----  cost = trivialCost
----}
----
----lT2 = L {
----  param = P p2,
----  implreq = \(P p) a -> ((a * p), (*p)),
----  upd = undefined,
----  cost = \(c, c') = 
----}
