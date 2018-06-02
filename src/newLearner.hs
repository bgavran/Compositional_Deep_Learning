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

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P

import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util
import CategoricDefinitions

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
  evalD :: a -> (b, DType a b) -- D a b is here instead of a -> b because sometimes we'd like to have higher order gradients
}

instance Category DType where
  type Allowed DType x = Additive x
  id      = D $ \a -> (a, id)
  D g . D f   = D $ \a -> let (b, f') = f a
                              (c, g') = g b
                          in (c, g' . f')

instance Monoidal DType where
  D f `x` D g = D $ \(a, b) -> let (c, f') = f a
                                   (d, g') = g b
                               in ((c, d), f' `x` g')

instance Cartesian DType where
  exl = D $ \(a, _) -> (a, exl)
  exr = D $ \(_, b) -> (b, exr)
  dup = D $ \a -> ((a, a), dup)

instance Cocartesian DType where
  type AllowedCoCar DType a = Additive a

  inl = D $ \a -> ((a, zero), inl)
  inr = D $ \b -> ((zero, b), inr)
  jam = D $ \(a, b) -> (a ^+ b, jam)

instance Closed DType where
  apply :: DType (DType a b, a) b
  apply = D $ \((D op), a) -> (fst $ op a, apply)

  curry :: (Additive3 a b c) => DType (a, b) c -> DType a (DType b c)
  curry d@(D op) = D $ \a -> (D $ \b -> let (c, op') = op (a, b)
                                        in (c, op' . inr), curry d)

  uncurry :: DType a (DType b c) -> DType (a, b) c
  uncurry d@(D op) = D $ \(a, b) -> let ((D bc), _) = op a
                                        (c     , _) = bc b
                                    in (c, uncurry d)

------------------------------------

instance {-# OVERLAPS #-} Additive a => Additive (DType a a) where 
-- does this instance even make sense? Perhaps just zero is needed and it's not "additive"
  zero = id
  one = undefined
  (^+) = undefined


applyF :: (x -> y) -> DType x y
applyF f = D $ \x -> (f x, applyF f) -- this is a linearD function from conal's paper?!
                                     -- this holds only for linear functions

newtype ParaType p a b = Para {
  evalP :: DType (Z p, a) b
}

-- this fn is useful both in parallel and sequential composition of Para
-- find a better name for this?
-- this is just like curryUncurry except it takes two functions and combines them?
compose fn f g = uncurry $ fn $ curry f `x` curry g

appliedFn :: (Additive4 a b c a1)
          => (a -> b -> c) -> DType a1 (a, b) -> DType a1 c
appliedFn fn = ((applyF (uncurry fn)) .)

-- this function is a really hacky thing to make instantiating parametrized category in this context possible
-- It's basically because composition in Category requires the parameter of codomain to be the same as in domain, 
-- but in this case the codomain's parameter is the product of domain parameters. I want to consider them the same, 
-- which is why I'm here taking their product and _then_ wrapping them in Z.
tupleToZ :: DType (Z p, Z p) b -> DType (Z p) b
tupleToZ (D op) = D $ \(p1 `X` p2) -> let (c, opD') = op (p1, p2)
                                       in (c, tupleToZ opD')

varToZ :: DType a b -> DType (Z a) b
varToZ (D op) = D $ \(P a) -> let (f, opD') = op a
                              in (f, varToZ opD')

-- What's the name of this pattern? this is moving the problem to another domain and then solving it there? 
curryUncurry :: (Closed k1, Closed k2, AllowedSeq k1 a1 b1 c1) 
             => (a1 `k1` (b1 `k1` c1) -> a2 `k2` (b2 `k2` c2)) -> (a1, b1) `k1` c1 -> (a2, b2) `k2` c2
curryUncurry f = uncurry . f . curry

type AllowedPara p x = Additive2 (Z p) x
type AllowedParaComp p a b c = (Additive3 a b c, Additive4 (Z p) (DType b c) (DType a b) (DType a c))

instance Category (ParaType p) where
  type Allowed (ParaType p) x = AllowedPara p x  
  type AllowedSeq (ParaType p) a b c = AllowedParaComp p a b c

  id = Para exr
  (Para g) . (Para f) = Para $ let fn = appliedFn (.)
                               in compose (tupleToZ . fn) g f

instance Monoidal (ParaType p) where
  type AllowedMon (ParaType p) a b c d = (AllowedParaComp p a b c, Additive3 d (DType b d) (DType (a, b) (c, d))) -- perhaps this can be reduced even further?

  (Para f) `x` (Para g) = Para $ let fn = appliedFn x in compose (tupleToZ . fn) f g

------------------------------------

newtype ContType k r a b = Cont ( (b `k` r) -> (a `k` r)) -- a -> b -> r

cont :: (Category k, AllowedSeq k a b r) => (a `k` b) -> ContType k r a b
cont f = Cont (. f)

instance Category k => Category (ContType k r) where
  type Allowed (ContType k r) a = Allowed k a
  id = Cont id
  Cont g . Cont f = Cont (f . g)

instance (Monoidal k, Cocartesian k) => Monoidal (ContType k r) where
  type AllowedMon (ContType k r) a b c d = (AllowedSeq k (a, b) (r, r) r, 
                                            AllowedSeq k c (c, d) r,
                                            AllowedSeq k d (c, d) r,
                                            AllowedMon k a b r r, 
                                            Allowed k r, 
                                            Allowed k c,
                                            Allowed k d,
                                            AllowedCoCar k c,
                                            AllowedCoCar k d,
                                            AllowedCoCar k r)
  (Cont f) `x` (Cont g) = Cont $ join . (f `x` g) . unjoin

instance (Cartesian k, Cocartesian k) => Cartesian (ContType k r) where
  type AllowedCar (ContType k r) a = (Allowed k a,
                                      AllowedSeq k a (a, a) r,
                                      AllowedCoCar k a
                                     )

  exl = Cont $ undefined
  exr = Cont $ undefined
  dup = Cont $ undefined

instance (Monoidal k, Cocartesian k) => Cocartesian (ContType k r) where
  type AllowedCoCar (ContType k r) a = (AllowedSeq k (a, a) (r, r) r,
                                        Allowed k r,
                                        AllowedMon k a a r r,
                                        AllowedCoCar k r,
                                        Allowed k a,
                                        AllowedCoCar k a)
  inl = Cont $ undefined
  inr = Cont $ undefined
  jam = Cont $ join . dup

------------------------------------

class HasDot s where
  dot :: u -> (u -> s)

------------------------------------

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

costGrad :: CostF b -> b
costGrad = undefined

-- cost fn requires continuations!
trivialCost :: CostF b
trivialCost = D $ \(b, b') -> (undefined, undefined)

instance Category (Learner p) where
  type Allowed (Learner p) x = AllowedPara p x
  type AllowedSeq (Learner p) a b c = AllowedParaComp p a b c

  id = L {param = NoP,
          implreq = id,
          upd    = \_ -> NoP,
          cost   = trivialCost}
 
  L p2 ir2 u2 c2 . L p1 ir1 u1 c1 = L {param = p2 `X` p1,
                                       implreq = ir2 . ir1, -- but this is not correct, yet! I need to integrate a cost function somewhere inside of this!
                                       upd = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
                                       cost = c2
  }
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

-- Is cost function a part of a learner? Cost function fits somewhere in between learners, when composing them? as part of learner composition?
-- Enriching the learners in Cost? But that wouldn't make sense since we need a cost for (B, B') prediction?

instance Monoidal (Learner p) where
  type AllowedMon (Learner p) a b c d = (AllowedParaComp p a b c, Additive3 d (DType b d) (DType (a, b) (c, d)))

  L p1 ir1 u1 c1 `x` L p2 ir2 u2 c2 = L {param = p1 `X` p2,
                                         implreq = ir1 `x` ir2,
                                         upd = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
                                         cost = compose (appliedFn x) c1 c2
  }

----------------------------------------

sgd :: (Num p, Fractional p) => p -> p -> p
sgd p pGrad = p - 0.001 * pGrad

--
--              
--sigm :: (Cat.Additive a, Floating a) => DType a a
--sigm = D $ \a -> let s = 1 / (1 + exp (-a))
--                 in (s, scale (s * (1 - s)))
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

df :: Additive a => DType a b -> a -> b
df opD v = let dD = dfD opD v
           in f dD one

---- Perhaps this dfn function is implemented incorrectly? Stuff with a -> b or b -> a seems fishy?
----dfn :: (_) => Int -> _ -> _ -> _
--dfn 0 op v = f op Cat.one
--dfn n op v = dfn (n - 1) (dfD op v) Cat.one
--
--allDerivs :: Cat.Additive a => DType a a -> a -> [a]
--allDerivs op v = f op v : df op v : map (\n -> dfn n op v) [2..7]
--

add :: Additive a => DType (a, a) a
add = jam

addn :: Double -> DType Double Double
addn n = D $ \a -> (a + n, id)

constD :: (Additive a, Num a) => a -> DType a a
constD k = D $ \_ -> (k, scale (zero))

----something :: (Additive a, Num a) => a -> DType a a
----something k = D $ \dm -> (k * dm, zeroD)
----

scale :: (Additive a, Num a) => a -> DType a a
scale k = D $ \a -> let f = (*k)  
                    in (f a, constD k) 

mul :: (Additive a, Num a) => DType (a, a) a
mul = D $ \(a, b) -> (a * b, scale b \/ scale a)

expD :: (Additive a, Floating a) => DType a a
expD = D $ \a -> let val = exp a
                 in (val, scale val)

sqr :: (Additive a, Num a) => DType a a
sqr = mul . dup

sqrError :: (Additive a, Num a) => DType (a, a) a
sqrError = sqr . (id \/ scale (-1))

leftToZ :: Additive3 p a b => DType (p, a) b -> DType (Z p, a) b
leftToZ = curryUncurry varToZ

zmul :: (Num a, Additive a) => ParaType a a a
zmul = Para $ leftToZ mul

zadd :: (Num a, Additive a) => ParaType a a a
zadd = Para $ leftToZ add

functorParaD :: ParaType p a b -> DType (Z p, a) b
functorParaD (Para f) = f

functorDPara :: Additive3 p a b => DType (p, a) b -> ParaType p a b
functorDPara = Para . leftToZ

------ This is basically a functor from Para -> Learn. It's defined by a cost and update function.
functorParaLearn :: ParaType p a b -> (p -> p -> p) -> CostF b -> Learner p a b
functorParaLearn para u c = L {
  param = undefined, -- initialized randomly in the shape of param?
  implreq = para,
  upd = \(p, pGrad) -> u <$> p <*> pGrad, -- just applying the update fn recursively to the param data stucture
  cost = c
}

l1 = functorParaLearn zmul sgd trivialCost

l2 = functorParaLearn zadd sgd sqrError

l3 = l2 . l1

l4 = l2 `x` l1

outputLearner l = let p  = param l
                      fl = (f . evalP . implreq) l
                  in (curry fl) p

---- Tensor manipulations
--
--ds # cs = listArray ds cs :: Array Double
--
--sh x = putStr P.. formatFixed 2 $ x
--
--p1 = [2, 3] # [0.2, 0.2..] ! "ij"
--p2 = [3, 5] # [0.1, 0.1..] ! "jk"
--
--p' = p1 * p2 -- 
--
--p3 = f mul (p1, p2) -- works out of the box with tensor product (since it's tensor product is a natural generalization of multiplication)
