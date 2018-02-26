{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Control.Category
import Control.Monad

type I p a b = (p -> a -> b)
type U p a b = (p -> a -> b -> p)
type R p a b = (p -> a -> b -> a)

implComp :: I p a b -> I q b c -> I (p, q) a c
implComp i j = \(p, q) a -> j q (i p a)

updComp :: U p a b -> U q b c -> b -> U (p, q) a c
updComp u v b = \(p, q) a c -> (u p a b, v q b c)

reqComp :: R p a b -> R q b c -> b -> R (p, q) a c
reqComp r s b = \(p, q) a c -> r p a (s q b c)


data LearnerType p a b = Learner {
  param :: p,
  impl :: p -> a -> b,
  upd :: p -> a -> b -> p,
  req :: p -> a -> b -> a
}

l1 = Learner {param = 3,
             impl = (\p a -> p * a), 
             upd = (\_ a _ -> a), 
             req = (\p _ _ -> p)}

l2 = Learner {param = 4,
             impl = (\p a -> p + a), 
             upd = (\_ a _ -> 1), 
             req = (\p _ _ -> 1)}

learnerComp :: LearnerType q b c -> LearnerType p a b -> LearnerType (p, q) a c
learnerComp l2 l1 = Learner {
                     param = (param l1, param l2),
                     impl = implComp (impl l1) (impl l2), 
                     upd = undefined, --updComp (upd l1) (upd l2) ((impl l1) (param l1) a),
                     req = undefined --reqComp (req l1) (req l2) ((impl l1) a)
                    }

instance Category (LearnerType p) where
  id = Learner {param = undefined,
                impl = (\_ a -> a),
                upd = (\p _ _ -> p),
                req = (\_ a _ -> a)}
  l2 . l1 = Learner {
                     param = undefined,
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

