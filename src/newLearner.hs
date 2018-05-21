{-# LANGUAGE 
             EmptyCase,
             FlexibleContexts,
             FlexibleInstances,
             InstanceSigs,
             MultiParamTypeClasses,
             LambdaCase,
             MultiWayIf,
             NamedFieldPuns,
             TupleSections,
             DeriveFunctor,
             TypeOperators,
             ScopedTypeVariables,
             ConstraintKinds,
             RankNTypes,
             NoMonomorphismRestriction,
             TypeFamilies 
                            #-}


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
  evalP :: DType (Z p, a) b
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

instance Cat.Closed DType where
  apply :: DType (a -> b, a) b
  apply = D $ \(f, a) -> (f a, applyGrad)
  
  curry :: DType (a, b) c -> DType a (b -> c)
  curry (D fTuple) = D $ \a -> (\b -> fst $ fTuple (a, b) , bcA)

  uncurry :: DType a (b -> c) -> DType (a, b) c
  uncurry (D fCurry)= D $ \(a, b) -> (fst (fCurry a) b, cBA)

  --uncurrying might be possible with recursive DTypes?

----------

c :: DType (DType a b, a) b
c = D $ \(dType, a) -> (f dType a, D $ \b' -> ((undefined, f (dfD dType a) b'), undefined))
--                                 D :: b (DType a b, a)

unc :: DType a (DType b c) -> DType (a, b) c
unc dFCurry = D $ \(a, b) -> (f (f dFCurry a) b, undefined)
--                                               D :: c (a, b)

----------

applyGrad :: DType b (a -> b, a)
applyGrad = D $ \b -> ((\a -> undefined, undefined), undefined)

fCurry :: a -> (DType b c, DType (DType b c) a)
fCurry = undefined

bcA :: DType (b -> c) a
bcA = undefined

cBA :: DType c (b, a)
cBA = undefined

fTuple :: (a, b) -> (c, DType c (a, b))
fTuple = undefined


type AdditiveABC a b c = (Cat.Additive a, Cat.Additive b, Cat.Additive c)

-- The type annotation can be even more general, but that's not needed for now.
(/\) :: AdditiveABC a b c => DType a b -> DType a c -> DType a (b, c)
f /\ g = (f `Cat.x` g) Cat.. Cat.dup 

(\/) :: AdditiveABC a b c => DType a c -> DType b c -> DType (a, b) c
f \/ g = Cat.jam Cat.. (f `Cat.x` g)

----------------------------------

instance Cat.Category (ParaType p) where
  id = let f = D $ \(_, a) -> (a, D $ \b' -> ((NoP, b'), f))
       in Para f
  (Para dg) . (Para df) = Para $ D $ \(p `X` q, a) -> let (b, f') = eval df (p, a)
                                                          (c, g') = eval dg (q, b)
                                                      in (c, D $ \c' -> let ((q', b'), g'') = eval g' c'
                                                                            ((p', a'), f'') = eval f' b'
                                                                        in ((p' `X` q', a'), evalP $ (Para g'') Cat.. (Para f'')))
--  (Para dg) . (Para df) = let h = D $ \(p `X` q, a) -> let newF = D $ \a -> undefined -- curry (eval df) p 
--                                                       in undefined
--                          in Para h

instance Cat.Monoidal (ParaType p) where
  (Para df) `x` (Para dg) = Para $ D $ \(p `X` q, (a, b)) -> let (c, f') = eval df (p, a)
                                                                 (d, g') = eval dg (q, b)
                                                             in ((c, d), D $ \(c', d') -> let ((p', a'), f'') = eval f' c'
                                                                                              ((q', b'), g'') = eval g' d'
                                                                                          in ((p' `X` q', (a', b')), evalP $ (Para f'') `Cat.x` (Para g'')))

----------------------------------

type ZF p = Z p
type ImplReqF p a b = ParaType p a b
type UpdF p = (Z p, Z p) -> Z p
type CostF b = DType (b, b) b -- (Predicted, True) - first is the output of the ir function, second one comes from learner 2

data Learner p a b = L {
  param :: ZF p,
  implreq  :: ImplReqF p a b,
  upd   :: UpdF p,
  cost  :: CostF b
}

instance Cat.Category (Learner p) where
  type Allowed (Learner p) x = Cat.Additive x
  id = L {param = NoP,
          implreq = Cat.id,
          upd    = \_ -> NoP,
          cost   = trivialCost}
 
  L p2 ir2' u2 c2 . L p1 ir1' u1 c1 = L {param = p1 `X` p2,
                                        implreq  = Para $ D $ \(p `X` q, a) -> let ir1 = evalP ir1'
                                                                                   ir2 = evalP ir2'
                                                                                   (b, f')  = eval ir1 (p, a) 
                                                                                   (c, g')  = eval ir2 (q, b)
                                                                                   costGrad = \b' -> snd $ f (dfD c1 (b, b')) Cat.one
                                                                               in (c, D $ \c' -> let ((q', b'), g'') = eval g' c'
                                                                                                     ((p', a'), f'') = ((eval f') . costGrad) b'
                                                                                                 in ((p' `X` q', a'), evalP $ (Para g'') Cat.. (Para f''))),
                                        upd   = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
                                        cost   = c2} 

------ Is cost function a part of a learner? Cost function fits somewhere in between learners, when composing them? as part of learner composition?

instance Cat.Monoidal (Learner p) where
  L p1 ir1 u1 c1 `x` L p2 ir2 u2 c2 = L {param = p1 `X` p2,
                                         implreq = ir1 `Cat.x` ir2,
                                         upd = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
                                         cost = D $ \((b, c), (b', c')) -> let (c1V, D c1') = eval c1 (b, b')
                                                                               (c2V, D c2') = eval c2 (c, c')
                                                                           in ((c1V, c2V), D $ \(c1V', c2V') -> let ((bP', bT'), c1'') = c1' c1V'
                                                                                                                    ((cP', cT'), c2'') = c2' c2V'
                                                                                                                in (((bP', cP'), (bT', cT')), undefined))
}

------------------------------------

sgd :: (Num p, Fractional p) => p -> p -> p
sgd p pGrad = p - 0.001 * pGrad

sqr :: (Cat.Additive a, Num a) => DType a a
sqr = mul Cat.. Cat.dup

sqrError :: (Cat.Additive a, Num a) => DType (a, a) a
sqrError = sqr Cat.. (Cat.id \/ scale (-1))

trivialCost :: CostF b
trivialCost = D $ \(b, b') -> (undefined, D $ \_ -> ((b, b'), undefined))

zeroD :: Cat.Additive a => DType a a
zeroD = D $ \_ -> (Cat.zero, zeroD)

add :: Cat.Additive a => DType (a, a) a
add = Cat.jam

zadd :: Num a => DType (Z a, a) a
zadd = D $ \(P a, b) -> (a + b, D $ \dm -> ((P dm, dm), undefined))

mul :: (Cat.Additive a, Num a) => DType (a, a) a
mul = D $ \(a, b) -> (a * b, scale b /\ scale a)

zmul :: Num a => DType (Z a, a) a
zmul = D $ \(P a, b) -> (a * b, D $ \dm -> ((P $ dm * b, dm * a), undefined))

something :: (Cat.Additive a, Num a) => a -> DType a a
something k = D $ \dm -> (k * dm, zeroD)

scale :: (Cat.Additive a, Num a) => a -> DType a a
scale k = D $ \a -> let f = (*k) 
                    in (f a, something k)
              
sigm :: (Cat.Additive a, Floating a) => DType a a
sigm = D $ \a -> let s = 1 / (1 + exp (-a))
                 in (s, undefined)
--D $ \dm -> (dm * s * (1 - s), undefined))

mm :: DType Double Double
mm = scale 4

f :: DType a b -> a -> b
f op v = fst $ eval op v

dfD :: DType a b -> a -> DType b a
dfD op v = snd $ eval op v 

df :: Cat.Additive b => DType a b -> a -> a
df op v = f (dfD op v) Cat.one

dfn :: Cat.Additive a => Int -> DType a a -> a -> a
dfn 0 op v = f op Cat.one
dfn n op v = dfn (n - 1) (dfD op v) Cat.one

paraFnMul :: ParaType Double Double Double
paraFnMul = Para zmul

paraFnAdd :: ParaType Double Double Double
paraFnAdd = Para zadd

---- This is basically a functor from Para -> Learn. Except we need to fix cost and update functions.
functorL :: ParaType p a b -> (p -> p -> p) -> CostF b -> Learner p a b
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

-- Tensor manipulations

ds # cs = listArray ds cs :: Array Double

sh x = putStr . formatFixed 2 $ x

p1 = [2, 3] # [0.1, 0.1..] ! "ij"
p2 = [3, 5] # [0.01, 0.01..] ! "jk"

p' = p1 * p2

-- Assumes single-letter index names
onesLike c = let d  = map iDim $ dims c
                 ch = concat $ map iName $ dims c
             in d # (repeat 1) ! ch

p1Grad = (onesLike p') * p2
p2Grad = (onesLike p') * p1
