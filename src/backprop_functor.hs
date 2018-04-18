{-# LANGUAGE TypeOperators #-}

import qualified CategoricDefinitions as Cat
import Control.Monad

data Param p
  = NoP
  | P p
  | X (Param p) (Param p) 
  deriving (Eq, Show)

data LearnerType p a b = Learner {
  param :: Param p,
  impl :: Param p -> a -> b,
  upd :: Param p -> a -> b -> Param p,
  req :: Param p -> a -> b -> a
}

instance Cat.Category (LearnerType p) where
  id = Learner {param = NoP,
                impl  = \_ a -> a,
                upd   = \_ _ _ -> NoP,
                req   = \_ a _ -> a}

  l2 . l1 = Learner {param = (param l1) `X` (param l2),
                     impl  = \(p `X` q) a   -> let b = (impl l1) p a
                                               in (impl l2) q b, 
                     upd   = \(p `X` q) a c -> let b = (impl l1) p a
                                               in ((upd l1) p a b) `X` ((upd l2) q b c),
                     req   = \(p `X` q) a c -> let b = (impl l1) p a
                                               in (req l1) p a ((req l2) q b c)}


instance Cat.Monoidal (LearnerType p) where
  l1 `x` l2 = Learner {param = (param l1) `X` (param l2),
                       impl  = \(p `X` q) (a, c)        -> ((impl l1) p a, (impl l2) q c),
                       upd   = \(p `X` q) (a, c) (b, d) -> ((upd l1) p a b) `X` ((upd l2) q c d),
                       req   = \(p `X` q) (a, c) (b, d) -> ((req l1) p a b, (req l2) q c d)}

-- No instance for Num a?
--instance Cat.Cartesian (LearnerType p) where
--  dup = Learner {param = NoP,
--                 impl  = \_ a -> (a, a),
--                 upd   = \_ _ _ -> NoP,
--                 req   = \_ a (b, c) -> b + c - a}

l1 = Learner {param = P 3,
             impl = \(P p) a -> p * a, 
             upd = \_ a _ -> P a, 
             req = \(P p) _ _ -> p}

l2 = Learner {param = P 5,
             impl = \(P p) a -> p + a, 
             upd = \_ a _ -> P 1, 
             req = \p _ _ -> 1}

lSerial = l2 Cat.. l1
lParallel = l2 `Cat.x` l1

f :: LearnerType p a b -> a -> b
f l a = (impl l) (param l) a

df :: LearnerType p a b -> a -> b -> (Param p)
df l a b = (upd l) (param l) a b

outSerial = f lSerial 2
outParallel = f lParallel (2, 3)

newP = (upd lSerial) (param lSerial) 2 1000
