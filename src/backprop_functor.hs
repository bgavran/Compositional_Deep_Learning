{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Control.Category
import Control.Monad

type I p a b = (p -> a -> b)
type U p a b = (p -> a -> b -> p)
type R p a b = (p -> a -> b -> a)

ijComp :: I p a b -> I q b c -> I (p, q) a c
ijComp i j = \(p, q) a -> j q (i p a)

uvComp :: U p a b -> U q b c -> b -> U (p, q) a c
uvComp u v b = \(p, q) a c -> (u p a b, v q b c)

rsComp :: R p a b -> R q b c -> b -> R (p, q) a c
rsComp r s b = \(p, q) a c -> r p a (s q b c)


--create :: p -> a -> b -> (p, I p a b, U p a b, R p a b)
--create p a b = (p, )

data Learner p a b = Learner {
  impl :: p -> a -> b,
  upd :: p -> a -> b -> p,
  req :: p -> a -> b -> a
}

a = Learner {impl = (*), 
             upd = (\_ a _ -> a), 
             req = (\p _ _ -> p)}

b = Learner {impl = (*), 
             upd = (\_ a _ -> a), 
             req = (\p _ _ -> p)}

--learnerComp :: Learner p a b -> Learner q b c -> a -> Learner (p, q) a c
--learnerComp (L p i u r) (L q j v s) a = L (p, q) 
--                                          (ijComp i j)
--                                          (uvComp u v (i p a)) 
--                                          (rsComp r s (i p a))


newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Category (Kleisli m) where
  id = Kleisli return
  (Kleisli f) . (Kleisli g) = Kleisli $ \x -> (f <=< g) x

instance Category (Learner g) where
  id = Learner {impl = (\_ a -> a),
                upd = (\p _ _ -> p),
                req = (\_ a _ -> a)}
--  l1@(Learner _ _ _) . l2@(Learner _ _ _) = Learner {
--                                    impl = ijComp (impl l1) (impl l2),
--                                    upd = uvComp (upd l1) (upd l2) (impl l1),
--                                    req = rsComp (req l1) (req l2) (impl l1)
--                                            }

--  ((L (p,i,u,r)) . (L (q,j,v,s))) a = L ((p, q) ,
--                                        (ijComp i j),
--                                        (uvComp u v (i p a)) ,
--                                        (rsComp r s (i p a)))

