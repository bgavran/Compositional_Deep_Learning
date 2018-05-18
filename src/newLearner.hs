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

newtype DType a b = D {
  eval :: a -> (b, DType b a) -- D b a is here instead of b -> a because sometimes we'd like to have higher order gradients
}

dConstr :: ((p -> a -> prod) -> (b, DType b (p -> a -> prod))) -> DType (p -> a -> prod) b
dConstr = D

newtype ParaType p a b = Para {
  evalPara :: DType (Z p, a) b
}

data RType p a b 
  = Rara (Z p) a b
  deriving (Eq, Show)

instance Cat.Category DType where
  type Allowed DType a = Cat.Additive a
  id      = D $ \a -> (a, Cat.id)
  (D g) . (D f)   = D $ \a -> let (b, f') = f a
                                  (c, g') = g b
                              in (c, f' Cat.. g')

instance Cat.Monoidal DType where
  (D f) `x` (D g) = D $ \(a, b) -> let (c, f') = f a
                                       (d, g') = g b
                                   in ((c, d), f' `Cat.x` g')

instance Cat.Cartesian DType where
  exl = D $ \(a, _) -> (a, Cat.inl)
  exr = D $ \(_, b) -> (b, Cat.inr)
  dup = D $ \a -> ((a, a), Cat.jam)

instance Cat.Cocartesian DType where
  inl = D $ \a -> ((a, Cat.zero), Cat.exl)
  inr = D $ \b -> ((Cat.zero, b), Cat.exr)
  jam = D $ \(a, b) -> (a Cat.^+ b, Cat.dup)

----------------------------------

a :: DType Double Double
a = scale 3

b :: Num a => DType (a -> b) b
b = D $ \f -> (f 2, undefined)

c :: Cat.Additive a => DType (DType a b) b
c = D $ \dType -> let val = Cat.one
                  in (f dType val, undefined) -- how to differentiate w.r.t a function? 

pp :: Num a => DType (Z a, a) a
pp = D $ \(P a, b) -> (a * b, D $ \dm -> ((P $ dm * b, dm * a), undefined))


instance Cat.Category (ParaType p) where
  id = let f = D $ \(_, a) -> (a, D $ \b' -> ((NoP, b'), f))
       in Para f
--  (Para dg) . (Para df) = let h = D $ \(p `X` q, a) -> let newF = D $ \a -> undefined -- curry (eval df) p 
--                                                       in undefined
--                          in Para h
  (Para dg) . (Para df) = let h = D $ \(p `X` q, a) -> let (b, f') = eval df (p, a)
                                                           (c, g') = eval dg (q, b)
                                                       in (c, D $ \c' -> let ((q', b'), g'') = eval g' c'
                                                                             ((p', a'), f'') = eval f' b'
                                                                         in ((p' `X` q', a'), evalPara $ (Para g'') Cat.. (Para f'')))
                          in Para h

instance Cat.Monoidal (ParaType p) where
  (Para df) `x` (Para dg) = let h = D $ \(p `X` q, (a, b)) -> let (c, f') = eval df (p, a)
                                                                  (d, g') = eval dg (q, b)
                                                              in ((c, d), D $ \(c', d') -> let ((p', a'), f'') = eval f' c'
                                                                                               ((q', b'), g'') = eval g' d'
                                                                                           in ((p' `X` q', (a', b')), evalPara $ (Para f'') `Cat.x` (Para g'')))
                            in Para h

----------------------------------

type ZF p = Z p
type ImplReqF p a b = ParaType p a b
type UpdF p = (Z p, Z p) -> Z p
type CostF b = DType (b, b) b

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

--cost :: (b, b) -> (b, (b, b))
 
--  L p2 ir2' u2 c2 . L p1 ir1' u1 c1 = let ir1 = evalPara ir1'
--                                          ir2 = evalPara ir2'
--                                      in L {param = p1 `X` p2,
--                                            implreq  = let h = D $ \(p `X` q, a) -> let (b, f')  = eval ir1 (p, a)
--                                                                                        (c, g')  = eval ir2 (q, b)
--                                                                                        costGrad = \b' -> snd $ df 1 c1 (b, b')
--                                                                                        --costGrad = \b' -> snd $ f (dfD c1 (b, b')) Cat.one
--                                                                                        --costGrad = \b' -> (snd . snd) $ c1 (b, b')
--                                                                                    in (c, D $ \c' -> let ((q', b'), g'') = eval g' c'
--                                                                                                          ((p', a'), f'') = ((eval f') . costGrad) b'
--                                                                                                      in ((p' `X` q', a'), evalPara $ (Para g'') Cat.. (Para f'')))
--                                                       in Para h,
--                                            upd   = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
--                                            cost   = c2} 

------ Is cost function a part of a learner? Cost function fits somewhere in between learners, when composing them? as part of learner composition?

--instance Cat.Monoidal (Learner p) where
--  L p1 ir1 u1 c1 `x` L p2 ir2 u2 c2 = L {param = p1 `X` p2,
--                                         implreq = ir1 `Cat.x` ir2,
--                                         upd = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
--                                        -- cost = c1 `X` c2
--                                         cost = \((b, c), (b', c')) -> let (b, (bTrue', bPred')) = c1 (b, b')
--                                                                           (c, (cTrue', cPred')) = c2 (c, c')
--                                                                       in ((b, c), ((bTrue', cTrue'), (bPred', cPred')))
--}
------------------------------------

sgd :: (Num p, Fractional p) => p -> p -> p
sgd p pGrad = p - 0.001 * pGrad


sqrError :: Num a => (a, a) -> (a, (a, a)) 
sqrError = \(c, c') -> ((c - c')^2, let v = 2 * (c - c')
                                    in (v, -v))

trivialCost :: CostF b
trivialCost = D $ \(b, b') -> (undefined, D $ \_ -> ((b, b'), undefined))

zeroD :: Cat.Additive a => DType a a
zeroD = let z = D $ \_ -> (Cat.zero, z)
        in z

add :: Cat.Additive a => DType (a, a) a
add = Cat.jam

zadd :: Num a => DType (Z a, a) a
zadd = D $ \(P a, b) -> (a + b, D $ \dm -> ((P dm, dm), undefined))

mul :: Num a => DType (a, a) a
mul = D $ \(a, b) -> (a * b, D $ \dm -> ((dm * b, dm * a), undefined))

zmul :: Num a => DType (Z a, a) a
zmul = D $ \(P a, b) -> (a * b, D $ \dm -> ((P $ dm * b, dm * a), undefined))

something :: Double -> DType Double Double
something k = D $ \dm -> (k * dm, zeroD)

scale :: Double -> DType Double Double
scale k = D $ \a -> let f = (*k) 
                    in (f a, something k)
              
sigm :: Floating a => DType a a
sigm = D $ \a -> let s = 1 / (1 + exp (-a))
                 in (s, D $ \dm -> (dm * s * (1 - s), undefined))

mm :: DType Double Double
mm = scale 4

f :: DType a b -> a -> b
f op v = fst $ eval op v

dfD :: DType a b -> a -> DType b a
dfD op v = snd $ eval op v 

df :: Cat.Additive a => Int -> DType a a -> a -> a
df 0 op v = f op Cat.one
df n op v = df (n - 1) (dfD op v) Cat.one

paraFnMul :: ParaType Double Double Double
paraFnMul = Para zmul

paraFnAdd :: ParaType Double Double Double
paraFnAdd = Para zadd

---- This is basically a functor from Para -> Learn. Except we need to fix cost and update functions.
--functorL :: Para p a b -> (p -> p -> p) -> CostF b -> Learner p a b
--functorL para u c = L {
--  param = undefined, -- initialized randomly in the shape of param?
--  implreq = para,
--  upd = \(p, pGrad) -> u <$> p <*> pGrad, -- just applying the update fn recursively to the param data stucture
--  cost = c
--}
--
--l1 = functorL paraFnMul sgd trivialCost
--
--l2 = functorL paraFnAdd sgd sqrError
--
--l3 = l2 Cat.. l1
--
--l4 = l2 `Cat.x` l1
--
---- Tensor manipulations
--
--ds # cs = listArray ds cs :: Array Double
--
--sh x = putStr . formatFixed 2 $ x
--
--p1 = [2, 3] # [0.1, 0.1..] ! "ij"
--p2 = [3, 5] # [0.01, 0.01..] ! "jk"
--
--c = p1 * p2
--
---- Assumes single-letter index names
--onesLike c = let d  = map iDim $ dims c
--                 ch = concat $ map iName $ dims c
--             in d # (repeat 1) ! ch
--
--p1Grad = (onesLike c) * p2
--p2Grad = (onesLike c) * p1
