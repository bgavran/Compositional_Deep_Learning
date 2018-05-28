{-# LANGUAGE 
             EmptyCase,
             FlexibleInstances,
             FlexibleContexts,
             InstanceSigs,
             MultiParamTypeClasses,
             PartialTypeSignatures,
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
             TypeFamilies,
             UndecidableInstances 
                            #-}


import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util
import qualified CategoricDefinitions as Cat

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
  eval :: a -> (b, DType a b) -- D b a is here instead of b -> a because sometimes we'd like to have higher order gradients
}

instance Cat.Category DType where
  type Allowed DType x = Cat.Additive x
  id      = D $ \a -> (a, Cat.id)
  D g . D f   = D $ \a -> let (b, f') = f a
                              (c, g') = g b
                          in (c, g' Cat.. f')

instance Cat.Monoidal DType where
  D f `x` D g = D $ \(a, b) -> let (c, f') = f a
                                   (d, g') = g b
                               in ((c, d), f' `Cat.x` g')

instance Cat.Cartesian DType where
  exl = D $ \(a, _) -> (a, Cat.exl)
  exr = D $ \(_, b) -> (b, Cat.exr)
  dup = D $ \a -> ((a, a), Cat.dup)

instance Cat.Cocartesian DType where
  inl = D $ \a -> ((a, Cat.zero), Cat.inl)
  inr = D $ \b -> ((Cat.zero, b), Cat.inr)
  jam = D $ \(a, b) -> (a Cat.^+ b, Cat.jam)

--instance Cat.Closed DType DType where
--  apply :: DType (DType a b, a) b
--  apply = D $ \(D op, a) -> let (b, op') = op a
--                            in (b, D $ \b' -> ((error "Can't differentiate w.r.t functions!", f op' b'), undefined))
----                                 D :: b  ->  (DType a b,                                    a)
-- 
--  curry :: (Additive3 a b c) => DType (a, b) c -> DType a (DType b c)
--  curry (D op) = D $ \a -> (D $ \b -> let (c, op') = op (a, b)
--                                      in (c, Cat.exr Cat.. op'), D $ \(D bc) -> let a' = undefined
--                                                                                in undefined) 
----                                                               D :: DType (DType b c) a
--  
--  uncurry :: DType a (DType b c) -> DType (a, b) c
--  uncurry (D op) = D $ \(a, b) -> let (D opbc, op') = op a     -- opbc :: b -> (c, DType c b), op' :: DType (DType b c) a
--                                      (c, D opbc')    = opbc b -- opbc' :: c -> (b, DType b c)
--                                  in (c, D $ \c' -> let (b', bc') = opbc' c'
--                                                    in ((undefined, b'), D undefined)) 
----                                       D :: c (a, b)                   D (a, b) c
------------

type Additive3 a b c = (Cat.Additive a, Cat.Additive b, Cat.Additive c)

------------------------------------
sqr :: (Cat.Additive a, Num a) => DType a a
sqr = mul Cat.. Cat.dup

sqrError :: (Cat.Additive a, Num a) => DType (a, a) a
sqrError = sqr Cat.. (Cat.id Cat.\/ scale (-1))

add :: Cat.Additive a => DType (a, a) a
add = Cat.jam

addn :: Double -> DType Double Double
addn n = D $ \a -> (a + n, Cat.id)

zeroD :: Cat.Additive a => DType a a
zeroD = D $ \_ -> (Cat.zero, zeroD)

constD :: Cat.Additive a => a -> DType a a
constD k = D $ \_ -> (k, zeroD)

----something :: (Cat.Additive a, Num a) => a -> DType a a
----something k = D $ \dm -> (k * dm, zeroD)
----
scale :: (Cat.Additive a, Num a) => a -> DType a a
scale k = D $ \a -> let f = (*k)  
                    in (f a, constD k) 

mul :: (Cat.Additive a, Num a) => DType (a, a) a
mul = D $ \(a, b) -> (a * b, scale b Cat.\/ scale a)

expD :: (Cat.Additive a, Floating a) => DType a a
expD = D $ \a -> let val = exp a
                 in (val, scale val)

------------------------------------

newtype ContType k r a b = Cont ( (b `k` r) -> (a `k` r)) -- a -> b -> r

cont :: (Cat.Category k, Cat.Allowed3 k a b r) => (a `k` b) -> ContType k r a b
cont f = Cont (Cat.. f)

instance Cat.Category k => Cat.Category (ContType k r) where
  type Allowed (ContType k r) a = Cat.Allowed k a
  id = Cont Cat.id
  Cont g . Cont f = Cont (f Cat.. g)

type MonType k a b c d r = (Cat.Cocartesian k, 
                            Cat.Monoidal k, 
                            Cat.AllowedSpecial1 k a b r,
                            Cat.AllowedSpecial2 k c d r)

x :: MonType k a b c d r => 
     ContType k r a c -> ContType k r b d -> ContType k r (a, b) (c, d)
Cont f `x` Cont g = Cont (Cat.join Cat.. (f `Cat.x` g) Cat.. Cat.unjoin)

--instance (Cat.Cocartesian k, Cat.Monoidal k, Cat.Allowed k (r, r), Cat.Allowed k r) => Cat.Monoidal (ContType k r) where
--  x :: (Cat.Allowed k (a, b), Cat.Allowed k (c, d), Cat.Allowed k c, Cat.Allowed k d) => 
--       ContType k r a c -> ContType k r b d -> ContType k r (a, b) (c, d)
--  Cont f `x` Cont g = x (Cont f) (Cont g)

------------------------------------
--
--newtype ParaType p a b = Para {
--  evalP :: DType (Z p, a) b
--}
--
--instance Cat.Category (ParaType p) where
--  id = let f = D $ \(_, a) -> (a, D $ \b' -> ((NoP, b'), f))       
--       in Para f
--
--  (Para dg) . (Para df) = Para $ D $ \(p `X` q, a) -> let (b, f') = eval df (p, a)
--                                                          (c, g') = eval dg (q, b)
--                                                      in (c, D $ \c' -> let ((q', b'), g'') = eval g' c'
--                                                                            ((p', a'), f'') = eval f' b'
--                                                                        in ((p' `X` q', a'), evalP $ (Para g'') Cat.. (Para f'')))
----  (Para dg) . (Para df) = let h = D $ \(p `X` q, a) -> let newF = D $ \a -> undefined -- curry (eval df) p 
----                                                       in undefined
----                          in Para h
--
--instance Cat.Monoidal (ParaType p) where
--  (Para df) `x` (Para dg) = Para $ D $ \(p `X` q, (a, b)) -> let (c, f') = eval df (p, a)
--                                                                 (d, g') = eval dg (q, b)
--                                                             in ((c, d), D $ \(c', d') -> let ((p', a'), f'') = eval f' c'
--                                                                                              ((q', b'), g'') = eval g' d'
--                                                                                          in ((p' `X` q', (a', b')), evalP $ (Para f'') `Cat.x` (Para g'')))
--
------------------------------------
--
--type ZF p = Z p
--type ImplReqF p a b = ParaType p a b
--type UpdF p = (Z p, Z p) -> Z p
--type CostF b = DType (b, b) b -- (Predicted, True) - first is the output of the ir function, second one comes from learner 2
--
--data Learner p a b = L {
--  param :: ZF p,
--  implreq  :: ImplReqF p a b,
--  upd   :: UpdF p,
--  cost  :: CostF b
--}
--
---- Can we curry learners? Make a learner that learns another learner? Is that meta learning? hmmm
--instance Cat.Category (Learner p) where
--  type Allowed (Learner p) x = Cat.Additive x
--  id = L {param = NoP,
--          implreq = Cat.id,
--          upd    = \_ -> NoP,
--          cost   = trivialCost}
-- 
--  L p2 ir2' u2 c2 . L p1 ir1' u1 c1 = L {param = p1 `X` p2,
--                                        implreq  = Para $ D $ \(p `X` q, a) -> let ir1 = evalP ir1'
--                                                                                   ir2 = evalP ir2'
--                                                                                   (b, f')  = eval ir1 (p, a) 
--                                                                                   (c, g')  = eval ir2 (q, b)
--                                                                                   costGrad = \b' -> snd $ f (dfD c1 (b, b')) Cat.one
--                                                                               in (c, D $ \c' -> let ((q', b'), g'') = eval g' c'
--                                                                                                     ((p', a'), f'') = ((eval f') . costGrad) b'
--                                                                                                 in ((p' `X` q', a'), evalP $ (Para g'') Cat.. (Para f''))),
--                                        upd   = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
--                                        cost   = c2} 
--
-------- Is cost function a part of a learner? Cost function fits somewhere in between learners, when composing them? as part of learner composition?
---- Enriching the learners in Cost? But that wouldn't make sense since we need a cost for (B, B') prediction?
--
--instance Cat.Monoidal (Learner p) where
--  L p1 ir1 u1 c1 `x` L p2 ir2 u2 c2 = L {param = p1 `X` p2,
--                                         implreq = ir1 `Cat.x` ir2,
--                                         upd = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
--                                         cost = D $ \((b, c), (b', c')) -> let (c1V, D c1') = eval c1 (b, b')
--                                                                               (c2V, D c2') = eval c2 (c, c')
--                                                                           in ((c1V, c2V), D $ \(c1V', c2V') -> let ((bP', bT'), c1'') = c1' c1V'
--                                                                                                                    ((cP', cT'), c2'') = c2' c2V'
--                                                                                                                in (((bP', cP'), (bT', cT')), undefined))
--}
--
--------------------------------------
--
--sgd :: (Num p, Fractional p) => p -> p -> p
--sgd p pGrad = p - 0.001 * pGrad
--
--trivialCost :: CostF b
--trivialCost = D $ \(b, b') -> (undefined, D $ \_ -> ((b, b'), undefined))
--
--              
--sigm :: (Cat.Additive a, Floating a) => DType a a
--sigm = D $ \a -> let s = 1 / (1 + exp (-a))
--                 in (s, scale (s * (1 - s)))
--
--
--add4 :: DType Double Double
--add4  = f (Cat.curry add) 4
--
--gg :: DType (Double, Double) Double
--gg = mul Cat.. (Cat.id `Cat.x` add4)
--
----D $ \dm -> (dm * s * (1 - s), undefined))
----
----dsigm :: (Floating a, Cat.Additive a) => DType a a
----dsigm = let s = 0.7
----            os = 1 - s
----        in f (Cat.curry mul) s
--
--
f :: DType a b -> a -> b
f (D op) v = fst $ op v

dfD :: DType a b -> a -> DType a b
dfD (D op) v = snd $ op v 

df :: Cat.Additive a => DType a b -> a -> b
df opD v = let dD = dfD opD v
           in f dD Cat.one

---- Perhaps this dfn function is implemented incorrectly? Stuff with a -> b or b -> a seems fishy?
----dfn :: (_) => Int -> _ -> _ -> _
--dfn 0 op v = f op Cat.one
--dfn n op v = dfn (n - 1) (dfD op v) Cat.one
--
--allDerivs :: Cat.Additive a => DType a a -> a -> [a]
--allDerivs op v = f op v : df op v : map (\n -> dfn n op v) [2..7]
--
--------------------------
--
--zadd :: Num a => DType (Z a, a) a
--zadd = D $ \(P a, b) -> (a + b, D $ \dm -> ((P dm, dm), undefined))
--
--zmul :: Num a => DType (Z a, a) a
--zmul = D $ \(P a, b) -> (a * b, D $ \dm -> ((P $ dm * b, dm * a), undefined))
--
--paraFnMul :: ParaType Double Double Double
--paraFnMul = Para zmul
--
--paraFnAdd :: ParaType Double Double Double
--paraFnAdd = Para zadd
--
--functorParaD :: ParaType p a b -> DType (Z p, a) b
--functorParaD (Para f) = f
--
--functorDPara :: DType a b -> p -> ParaType p a b
--functorDPara (D f) p = undefined
----        f :: a -> (b, D b a)
--
------ This is basically a functor from Para -> Learn. Except we need to fix cost and update functions.
--functorParaLearn :: ParaType p a b -> (p -> p -> p) -> CostF b -> Learner p a b
--functorParaLearn para u c = L {
--  param = undefined, -- initialized randomly in the shape of param?
--  implreq = para,
--  upd = \(p, pGrad) -> u <$> p <*> pGrad, -- just applying the update fn recursively to the param data stucture
--  cost = c
--}
--
--l1 = functorParaLearn paraFnMul sgd trivialCost
--
--l2 = functorParaLearn paraFnAdd sgd sqrError
--
--l3 = l2 Cat.. l1
--
--l4 = l2 `Cat.x` l1
--
-- Tensor manipulations

ds # cs = listArray ds cs :: Array Double

sh x = putStr . formatFixed 2 $ x

p1 = [2, 3] # [0.2, 0.2..] ! "ij"
p2 = [3, 5] # [0.1, 0.1..] ! "jk"

p' = p1 * p2 -- 

--p3 = f mul (p1, p2) -- works out of the box with tensor product (since it's tensor product is a natural generalization of multiplication)
