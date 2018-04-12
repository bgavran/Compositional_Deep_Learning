{-# LANGUAGE TypeOperators #-}

import qualified Control.Category as Cat
import Control.Monad

data MyParam p
  = NoParam
  | Param p
  | ParamProd (MyParam p) (MyParam p) 
  deriving (Eq, Show)

data LearnerType p a b = Learner {
  param :: MyParam p,
  impl :: MyParam p -> a -> b,
  upd :: MyParam p -> a -> b -> MyParam p,
  req :: MyParam p -> a -> b -> a
}

instance Cat.Category (LearnerType p) where
  id = Learner {param = undefined,
                impl = \_ a -> a,
                upd = \p _ _ -> p,
                req = \_ a _ -> a}

  l2 . l1 = Learner {param = ParamProd (param l1) (param l2),
                     impl = \(ParamProd p q) a -> let b = (impl l1) p a
                                                  in (impl l2) q b, 
                     upd = \(ParamProd p q) a c -> let b = (impl l1) p a
                                                   in ParamProd ((upd l1) p a b) ((upd l2) q b c),
                     req = \(ParamProd p q) a c -> let b = (impl l1) p a
                                                   in (req l1) p a ((req l2) q b c)}

monProd :: LearnerType p c d -> LearnerType p a b -> LearnerType p (a, c) (b, d)
monProd l2 l1 = Learner {param = ParamProd (param l1) (param l2),
                         impl = \(ParamProd p q) (a, c) -> ((impl l1) p a, (impl l2) q c),
                         upd = \(ParamProd p q) (a, c) (b, d) -> ParamProd ((upd l1) p a b) ((upd l2) q c d),
                         req = \(ParamProd p q) (a, c) (b, d) -> ((req l1) p a b, (req l2) q c d)}

etaVal = 0.01

bias = Learner {param = Param 1,
                impl = \(Param p) _ -> p,
                upd = \(Param p) a b -> Param $ sgd p ((f bias a) - b) etaVal,
                req = \_ _ _ -> 0}

fSigm :: Double -> Double
fSigm x = 1 / (1 + (exp $ negate x))

dfSigm :: Double -> Double
dfSigm x = (fSigm x) * (1 - fSigm x)

sigmoid = Learner {param = NoParam,
                   impl = \_ a -> fSigm a,
                   upd = \_ _ _ -> NoParam,
                   req = \_ a b -> a - (a - b) * dfSigm a}

sgd :: Double -> Double -> Double -> Double
sgd p pGrad eta = p - eta * pGrad

scalarMul = Learner {param = Param 2,
                     impl = \(Param p) a -> p * a,
                     upd = \(Param p) a b -> Param $ sgd p (a*((f scalarMul a) - b)) etaVal,
                     req = \(Param p) a b -> sgd a (p*((f scalarMul a) - b)) etaVal}

unit = Learner {param = Param 1,
               impl = \(Param p) _ -> p,
               upd = \(Param p) _ b -> Param p,
               req = \_ _ _ -> 0 }

mul = Learner {param = NoParam,
              impl = \_ (f, s) -> f + s,
              upd = \_ _ _ -> NoParam,
              req = \_ a b -> a}

l1 = Learner {param = Param 3,
             impl = \(Param p) a -> p * a, 
             upd = \_ a _ -> Param a, 
             req = \(Param p) _ _ -> p}

l2 = Learner {param = Param 5,
             impl = \(Param p) a -> p + a, 
             upd = \_ a _ -> Param 1, 
             req = \p _ _ -> 1}

l3 = Learner {param = NoParam,
              impl = \_ a -> exp a,
              upd = \_ _ _ -> NoParam,
              req = \p a b -> b * ((impl l3) p a)}

lSerial = l2 Cat.. l1
lParallel = l2 `monProd` l1

f :: LearnerType p a b -> a -> b
f l a = (impl l) (param l) a

df :: LearnerType p a b -> a -> b -> (MyParam p)
df l a b = (upd l) (param l) a b

outSerial = f lSerial 2
outParallel = f lParallel (2, 3)

newP = (upd lSerial) (param lSerial) 2 1000
