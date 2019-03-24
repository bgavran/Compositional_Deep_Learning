module Autodiff.Additive where

import Prelude hiding (id, (.))
import Control.Lens hiding ((#), para)
import qualified Prelude as P
import GHC.Exts (Constraint)

import CategoricDefinitions
import OnesLike

{-
The sole purpose of this is to wrap around (->) and provide a Cocartesian instance (which (->) doesn't have)
-}
newtype a ->+ b = AddFun {
    _evalAF :: a -> b
}
makeLenses ''(->+)

instance Category (->+) where
    type Allowed (->+) a = Additive a
    id = AddFun id
    AddFun g . AddFun f = AddFun (g . f)

instance Monoidal (->+) where
    AddFun f `x` AddFun g = AddFun (f `x` g)
    assocL = AddFun assocL
    assocR = AddFun assocR
    unitorL = AddFun unitorL
    unitorL' = AddFun unitorL'
    swap = AddFun swap

instance Cartesian (->+) where
    exl = AddFun exl
    exr = AddFun exr
    dup = AddFun dup
    counit = AddFun counit

instance Cocartesian (->+) where
    inl = AddFun inlF
    inr = AddFun inrF
    jam = AddFun jamF
    unit = AddFun unitF

instance Num a => Scalable (->+) a where
    scale a = AddFun (*a)

instance Num a => NumCat (->+) a where
    negateC = AddFun negateC
    addC = AddFun addC
    mulC = AddFun mulC
    increaseC a = AddFun (increaseC a)

instance Floating a => FloatCat (->+) a where
    expC = AddFun expC

instance Fractional a => FractCat (->+) a where
    recipC = AddFun recipC

-----------------------------

inlF :: Additive b => a -> (a, b)
inrF :: Additive a => b -> (a, b)
jamF :: Additive a => (a, a) -> a
unitF :: Additive a => () -> a

inlF = \a -> (a, zero)
inrF = \b -> (zero, b)
jamF = \(a, b) -> a ^+ b
unitF = \_ -> zero -- this should probably be onesLike?

type family AllAdditive xs :: Constraint where
    AllAdditive '[] = ()
    AllAdditive (x : xs) = (Additive x, AllAdditive xs)
