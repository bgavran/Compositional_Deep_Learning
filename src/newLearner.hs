{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

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
  eval :: a -> (b, b -> a)
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
  exl = D $ \(a, _) -> (a, Cat.inlF)
  exr = D $ \(_, b) -> (b, Cat.inrF)
  dup = D $ \a -> ((a, a), Cat.jamF)

--------------------------------

type ZF p = Z p
type ImplReqF p a b = D (Z p, a) (b)
type UpdF p = (Z p, Z p) -> Z p
type CostF b = (b, b) -> (b, b) -- outputs the tuple of cost and the gradient of cost (previous_grad is 1)

data Learner p a b = L {
  param :: ZF p,
  implreq  :: ImplReqF p a b,
  upd   :: UpdF p,
  cost  :: CostF b
}

trivialCost :: CostF b
trivialCost (_, b') = (undefined, b')

instance Cat.Category (Learner p) where
  id = L {param = NoP,
          implreq  = D $ \(_, a)  -> (a, \b -> (NoP, b)),
          upd    = \_ -> NoP,
          cost   = undefined}

  L p2 ir2 u2 c2 . L p1 ir1 u1 c1 = L {param = p1 `X` p2,
                                       implreq  = D $ \(p `X` q, a)   -> let (b, f')  = eval ir1 (p, a)
                                                                             (c, g')  = eval ir2 (q, b)
                                                                             costGrad = \b' -> snd $ c1 (b, b')
                                                                          in (c, \c' -> let (p2, b') = g' c'
                                                                                            (p1, a') = (f' . costGrad) b'
                                                                                        in (p1 `X` p2, a')), 
                                       upd   = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
                                       cost   = c2} 

-- Is cost function a part of a learner? Cost function fits somewhere in between learners, when composing them? as part of learner composition?

instance Cat.Monoidal (Learner p) where
  L p1 ir1 u1 c1 `x` L p2 ir2 u2 c2 = L {param = p1 `X` p2,
                                         implreq = D $ \(p `X` q, (a, c))  -> let (b, f') = eval ir1 (p, a)
                                                                                  (d, g') = eval ir2 (q, c)
                                                                              in ((b, d), \(b', d') -> let (p1, a') = f' b'
                                                                                                           (p2, c') = g' d'
                                                                                                       in (p1 `X` p2, (a', c'))),
                                         upd = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
                                         cost = \((b, c), (b', c')) -> let (b, bGrad) = c1 (b, b')
                                                                           (c, cGrad) = c2 (c, c')
                                                                       in ((b, c), (bGrad, cGrad))}

sgd :: (Num p, Fractional p) => p -> p -> p
sgd p pGrad = p - 0.001 * pGrad

l1 = L {
  param = P (2 :: Double),
  implreq = D $ \(P p, a) -> (a * p, \b' -> (P (a * b'), b' * p)),
  upd = \(p, pGrad) -> sgd <$> p <*> pGrad, 
  cost = trivialCost
}

l2 = L {
  param = P (3 :: Double),
  implreq = D $ \(P p, a) -> (a + p, \b' -> (P b', b')),
  upd = \(p, pGrad) -> sgd <$> p <*> pGrad,
  cost = \(c, c') -> ((c - c')^2, 2*(c - c'))
}

l3 = l2 Cat.. l1

l4 = l2 `Cat.x` l1

ir :: Learner p a b -> a -> (b, b -> (Z p, a))
ir l a = (eval $ implreq l) (param l, a)

f :: Learner p a b -> a -> b
f l a = fst $ ir l a

df :: Learner p a b -> a -> (b -> (Z p, a))
df l a = snd $ ir l a

learnerGrad :: Learner Double Double Double -> Double -> (Z Double, Double)
learnerGrad l = df l 10

updateLearner :: Learner Double Double Double -> Learner Double Double Double
updateLearner l = let pGrad = fst $ (learnerGrad l) 1 
                  in l {param = (upd l) (param l, pGrad) }

--- Tensor manipulations

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

--lT1 = L {
--  param = P p1,
--  implreq = \(P p) a -> ((a * p), (*p)),
--  upd = undefined,
--  cost = trivialCost
--}
--
--lT2 = L {
--  param = P p2,
--  implreq = \(P p) a -> ((a * p), (*p)),
--  upd = undefined,
--  cost = \(c, c') = 
--}
