{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Control.Category
import Control.Monad

data LearnerType p a b = Learner {
  impl :: p -> a -> b,
  upd :: p -> a -> b -> p,
  req :: p -> a -> b -> a
}


{- 
 composition of impl, upd and req can't be defined outside of Learner 
 since we need to define let b = ... 
-}
learnerComp :: LearnerType q b c -> LearnerType p a b -> LearnerType (p, q) a c
learnerComp l2 l1 = Learner {impl = \(p, q) a -> (impl l2) q ((impl l1) p a), 
                             upd = \(p, q) a c -> let b = (impl l1) p a
                                                  in ((upd l1) p a b, (upd l2) q b c),
                             req = \(p, q) a c -> let b = (impl l1) p a
                                                  in (req l1) p a ((req l2) q b c)
}

l1 = Learner {impl = \p a -> p * a, 
             upd = \_ a _ -> a, 
             req = \p _ _ -> p}

l2 = Learner {impl = \p a -> p + a, 
             upd = \_ a _ -> 1, 
             req = \p _ _ -> 1}

l3 = learnerComp l2 l1

c = (impl l3) (2, 3) 4 -- super interesting :)



instance Category (LearnerType p) where
  id = Learner {impl = (\_ a -> a),
                upd = (\p _ _ -> p),
                req = (\_ a _ -> a)}
  (.) = undefined --learnerComp 

 
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

