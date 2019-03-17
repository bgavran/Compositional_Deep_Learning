module Autodiff.Additive where

import Prelude hiding (id, (.))
import Control.Lens hiding ((#), para)
import qualified Prelude as P

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

type Additive2 a b = (Additive a, Additive b)
type Additive3 a b c = (Additive2 a b, Additive c)
type Additive4 a b c d = (Additive3 a b c, Additive d)
type Additive5 a b c d e = (Additive4 a b c d, Additive e)
type Additive6 a b c d e f = (Additive5 a b c d e, Additive f)
type Additive7 a b c d e f g = (Additive6 a b c d e f, Additive g)
type Additive8 a b c d e f g h = (Additive7 a b c d e f g, Additive h)
type Additive9 a b c d e f g h i = (Additive8 a b c d e f g h , Additive i)
