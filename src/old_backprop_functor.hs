{-# LANGUAGE TypeOperators #-}

import qualified CategoricDefinitions as Cat
import Control.Monad

data Param p
  = NoP
  | P p
  | X (Param p) (Param p) 
  deriving (Eq, Show)

type ParamF p     = Param p
type ImplF  p a b = Param p -> a -> b
type UpdF   p a b = Param p -> a -> b -> Param p
type ReqF   p a b = Param p -> a -> b -> a

data Learner p a b = L {
  param :: ParamF p,
  impl :: ImplF p a b,
  upd :: UpdF p a b,
  req :: ReqF p a b
}

instance Cat.Category (Learner p) where
  id = L {param = NoP,
          impl  = \_ a   -> a,
          upd   = \_ _ _ -> NoP,
          req   = \_ a _ -> a}

  L p2 i2 u2 r2 . L p1 i1 u1 r1 = L {param = p1 `X` p2,
                                     impl  = \(p `X` q) a   -> let b = i1 p a
                                                               in i2 q b, 
                                     upd   = \(p `X` q) a c -> let b = i1 p a
                                                               in (u1 p a b) `X` (u2 q b c),
                                     req   = \(p `X` q) a c -> let b = i1 p a
                                                               in r1 p a (r2 q b c)}


instance Cat.Monoidal (Learner p) where
  L p1 i1 u1 r1 `x` L p2 i2 u2 r2 = L {param = p1 `X` p2,
                                       impl  = \(p `X` q) (a, c)        -> (i1 p a, i2 q c),
                                       upd   = \(p `X` q) (a, c) (b, d) -> (u1 p a b) `X` (u2 q c d),
                                       req   = \(p `X` q) (a, c) (b, d) -> (r1 p a b, r2 q c d)}

-------------

data ParaType p n m = Para {
  paramP :: ParamF p,
  implP :: ImplF p n m
}

instance Cat.Category (ParaType p) where
  id = Para {paramP = NoP,
             implP  = \_ a -> a}
 
  Para p2 i2 . Para p1 i1 = Para {paramP = p1 `X` p2,
                                  implP  = \(p `X` q) a -> i2 q (i1 p a)}

instance Cat.Monoidal (ParaType p) where
  Para p1 i1 `x` Para p2 i2 = Para {paramP = p1 `X` p2,
                                    implP  = \(p `X` q) (a, c) -> (i1 p a, i2 q c)}

-------------


functorL :: ParaType p n m -> Learner p n m
functorL (Para p i) = L {param = p,
                         impl  = i,
                         upd   = undefined,
                         req   = undefined}

------------
eps :: Double
eps = 0.001

-- alpha is defined in appendix B as a function?
alpha :: Int -> Double
alpha = undefined

-- Loss function - has to be differentiable
-- Authors define e as a function that takes two real numbers? 
-- instead of as taking a more abstract type?
type Loss b = b -> b -> Double

e :: Loss b
e = undefined

-- According to the paper, this needs to be multiplied by alpha_B, which is never actually defined?
-- The paper doesn't impose any method of calculating the derivative of a composition of functions for a Learner.
-- What is the best way to calcualate those derivatives dynamically?
-- How can this be combined with the automatic differentiation approach?
costFn :: ImplF p a b -> Loss b -> (Param p) -> [(a, b)] -> Double
costFn i loss p = sum . map (\(a, b) -> e (i p a) b)

--costFnDeriv :: 

-------------

l1 = L {param = P 3,
        impl = \(P p) a -> p * a, 
        upd = \_ a _ -> P a, 
        req = \(P p) _ _ -> p} 

l2 = L {param = P 5,
        impl = \(P p) a -> p + a, 
        upd = \_ a _ -> P 1, 
        req = \p _ _ -> 1}

lSerial = l2 Cat.. l1
lParallel = l2 `Cat.x` l1

f :: Learner p a b -> a -> b
f l a = (impl l) (param l) a

df :: Learner p a b -> a -> b -> (Param p)
df l a b = (upd l) (param l) a b

outSerial = f lSerial 2
outParallel = f lParallel (2, 3)

newP = (upd lSerial) (param lSerial) 2 1000
