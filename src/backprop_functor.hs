{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Control.Category
import Control.Monad

type I p a b = (p -> a -> b)
type U p a b = (p -> a -> b -> p)
type R p a b = (p -> a -> b -> a)

ijComp :: (p -> a -> b) -> (q -> b -> c) -> ((p, q) -> a -> c)
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

l1 = Learner {impl = (*), 
             upd = (\_ a _ -> a), 
             req = (\p _ _ -> p)}

l2 = Learner {impl = (*), 
             upd = (\_ a _ -> a), 
             req = (\p _ _ -> p)}

instance Category (Learner g) where
  id = Learner {impl = (\_ a -> a),
                upd = (\p _ _ -> p),
                req = (\_ a _ -> a)}

  l2 . l1 = Learner {
                     impl = undefined,
                     upd = undefined,
                     req = undefined
                    }

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

