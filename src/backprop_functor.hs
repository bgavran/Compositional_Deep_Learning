{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Control.Category
import Control.Monad

type I p a b = (MyParam p -> a -> b)
type U p a b = (MyParam p -> a -> b -> MyParam p)
type R p a b = (MyParam p -> a -> b -> a)


reduceParam :: MyParam p -> MyParam q-> MyParam (p,q)
reduceParam (Param p) (Param q) = Param (p,q)

implComp :: I p a b -> I q b c -> I (p,q) a c
implComp i j = \(Param (p,q)) a -> j (Param q) (i (Param p) a)

updComp :: U p a b -> U q b c -> b -> U (p, q) a c
updComp u v b = \(Param (p,q)) a c -> reduceParam (u (Param p) a b) (v (Param q) b c)

reqComp :: R p a b -> R q b c -> b -> R (p,q) a c
reqComp r s b = \(Param (p,q)) a c -> r (Param p) a (s (Param q) b c)


data MyParam p
  = Param p 
  deriving (Eq, Show)

data Learner p a b = Learner {
  impl :: (MyParam p) -> a -> b,
  upd :: (MyParam p) -> a -> b -> (MyParam p),
  req :: (MyParam p) -> a -> b -> a
}

l1 = Learner {impl = (\(Param p) a -> p * a), 
             upd = (\_ a _ -> (Param a)), 
             req = (\(Param p) _ _ -> p)}

l2 = Learner {impl = (\(Param p) a -> p + a), 
             upd = (\_ a _ -> (Param 1)), 
             req = (\(Param p) _ _ -> 1)}

instance Category (Learner p) where
  id = Learner {impl = (\_ a -> a),
                upd = (\p _ _ -> p),
                req = (\_ a _ -> a)}
  l2 . l1 = Learner {
                     impl = undefined, --(impl l1) `implComp` (impl l2), 
                     upd = undefined,
                     req = undefined
                    }

--instance Cat Learner where
--  objid = Learner {impl = (\_ a -> a),
--                upd = (\p _ _ -> p),
--                req = (\_ a _ -> a)}
--
--  l2 `objcomp` l1 = Learner {
--                     impl = undefined,
--                     upd = undefined,
--                     req = undefined
--                    }

class Cat obj where
  id :: obj a -> obj a
  (.) :: (obj b -> obj c) -> (obj a -> obj b) -> obj a -> obj c

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Category (Kleisli m) where
  id = Kleisli return
  (Kleisli f) . (Kleisli g) = Kleisli $ \x -> (f <=< g) x



--learnerComp :: Learner p a b -> Learner q b c -> a -> Learner (p, q) a c
--learnerComp (L p i u r) (L q j v s) a = L (p, q) 
--                                          (ijComp i j)
--                                          (uvComp u v (i p a)) 
--                                          (rsComp r s (i p a))



--  ((L (p,i,u,r)) . (L (q,j,v,s))) a = L ((p, q) ,
--                                        (ijComp i j),
--                                        (uvComp u v (i p a)) ,
--                                        (rsComp r s (i p a)))

