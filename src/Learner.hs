{-#r LANGUAGE 
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

--------------------------------------
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
--costGrad :: CostF b -> b
--costGrad = undefined
--
---- cost fn requires continuations!
--trivialCost :: CostF b
--trivialCost = D $ \(b, b') -> (undefined, undefined)
--
--instance Category (Learner p) where
--  type Allowed (Learner p) x = AllowedPara p x
--  type AllowedSeq (Learner p) a b c = AllowedParaComp p a b c
--
--  id = L {param = NoP,
--          implreq = id,
--          upd    = \_ -> NoP,
--          cost   = trivialCost}
-- 
--  L p2 ir2 u2 c2 . L p1 ir1 u1 c1 = L {param = p2 `X` p1,
--                                       implreq = ir2 . ir1, -- but this is not correct, yet! I need to integrate a cost function somewhere inside of this!
--                                       upd = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
--                                       cost = c2
--  }
----  L p2 ir2' u2 c2 . L p1 ir1' u1 c1 = L {param = p1 `X` p2,
----                                        implreq  = Para $ D $ \(p `X` q, a) -> let ir1 = evalP ir1'
----                                                                                   ir2 = evalP ir2'
----                                                                                   (b, f')  = eval ir1 (p, a) 
----                                                                                   (c, g')  = eval ir2 (q, b)
----                                                                                   costGrad = \b' -> snd $ f (dfD c1 (b, b')) Cat.one
----                                                                               in (c, D $ \c' -> let ((q', b'), g'') = eval g' c'
----                                                                                                     ((p', a'), f'') = ((eval f') . costGrad) b'
----                                                                                                 in ((p' `X` q', a'), evalP $ (Para g'') Cat.. (Para f''))),
----                                        upd   = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
----                                        cost   = c2} 
--
---- Is cost function a part of a learner? Cost function fits somewhere in between learners, when composing them? as part of learner composition?
--
--instance Monoidal (Learner p) where
--  type AllowedMon (Learner p) a b c d = (AllowedParaComp p a b c, Additive3 d (DType b d) (DType (a, b) (c, d)))
--
--  L p1 ir1 u1 c1 `x` L p2 ir2 u2 c2 = L {param = p1 `X` p2,
--                                         implreq = ir1 `x` ir2,
--                                         upd = \(p `X` q, pGrad `X` qGrad) -> u1 (p, pGrad) `X` u2 (q, qGrad),
--                                         cost = let fn = appliedFn (uncurry x)
--                                                in curryUncurry2 fn c1 c2
--  }
