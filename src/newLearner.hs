{-# LANGUAGE TypeOperators #-}

import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util
import qualified CategoricDefinitions as Cat
import Control.Monad

data Z p
  = NoP
  | P p
  | X (Z p) (Z p) 
  deriving (Eq, Show)

type ZF p = Z p
type ImplReqF p a b = (Z p, a) -> (b, b -> (Z p, a))
type UpdF p = (Z p, Z p) -> Z p
type CostF b = (b, b) -> (b, b) -- outputs the cost and the gradient of cost (previous_grad is 1)

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
          implreq  = \(_, a)  -> (a, \b -> (NoP, b)),
          upd    = \_ -> NoP,
          cost   = undefined}

  L p2 ir2 u2 c2 . L p1 ir1 u1 c1 = L {param = p1 `X` p2,
                                       implreq  = \(p `X` q, a)   -> let (b, f')  = ir1 (p, a)
                                                                         (c, g')  = ir2 (q, b)
                                                                         costGrad = \b' -> snd $ c1 (b, b')
                                                                     in (c, \c' -> let (p2, b') = g' c'
                                                                                       (p1, a') = (f' . costGrad) b'
                                                                                   in (p1 `X` p2, a')), 
                                       upd   = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
                                       cost   = c2} 

-- Is cost function a part of a learner? Cost function fits somewhere in between learners, when composing them? as part of learner composition?

instance Cat.Monoidal (Learner p) where
  L p1 ir1 u1 c1 `x` L p2 ir2 u2 c2 = L {param = p1 `X` p2,
                                         implreq = \(p `X` q, (a, c))  -> let (b, f') = ir1 (p, a)
                                                                              (d, g') = ir2 (q, c)
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
  implreq = \(P p, a) -> (a * p, \b' -> (P (a * b'), b' * p)),
  upd = undefined, 
  cost = trivialCost
}

l2 = L {
  param = P (3 :: Double),
  implreq = \(P p, a) -> (a + p, \b' -> (P b', b')),
  upd = undefined,
  cost = \(c, c') -> ((c - c')^2, 2*(c - c'))
}

l3 = l2 Cat.. l1

l4 = l2 `Cat.x` l1

ir :: Learner p a b -> a -> (b, b -> (Z p, a))
ir l a = (implreq l) (param l, a)

f :: Learner p a b -> a -> b
f l a = fst $ ir l a

df :: Learner p a b -> a -> (b -> (Z p, a))
df l a = snd $ ir l a

l3Grad :: Double -> (Z Double, Double)
l3Grad = df l3 10

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
