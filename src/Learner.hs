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

module Learner where

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P

import CategoricDefinitions
import AD

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
  

newtype ParaType p a b = Para {
  evalP :: DType (Z p, a) b
}

-- this fn is useful both in parallel and sequential composition of Para
-- find a better name for this?
-- this is just like curryUncurry except it takes two functions and combines them?
-- possible to do this in pointfree style?
compose fn f g = uncurry $ fn $ curry f `x` curry g

-- What's the name of this pattern? this is moving the problem to another domain and then solving it there? 
curryUncurry :: (Closed k1, Closed k2, AllowedSeq k1 a1 b1 c1) 
             => (a1 `k1` (b1 `k1` c1) -> a2 `k2` (b2 `k2` c2)) -> (a1, b1) `k1` c1 -> (a2, b2) `k2` c2
curryUncurry f = uncurry . f . curry


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


appliedFn :: (Additive4 a b c a1)
          => (a -> b -> c) -> DType a1 (a, b) -> DType a1 c
appliedFn fn = ((linearD (uncurry fn)) .)

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

