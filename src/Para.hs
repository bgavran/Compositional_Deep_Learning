module Para where

import Prelude hiding (id, (.))
import Control.Lens hiding ((#), para)

import CategoricDefinitions
import Autodiff.GAD
import Autodiff.Additive
import Autodiff.Dual
import Autodiff.DType
import TensorUtils

-------------------------------------------------------------------

data ParaType p a b = Para {
    _param :: p,
    _fn :: DType (p, a) b
}
makeLenses ''ParaType

-- Sequential composition of parametrized functions
(.<<) :: (_)
    => ParaType q b c
    -> ParaType p a b
    -> ParaType (p, q) a c
(Para q g) .<< (Para p f) = Para (p, q) (g .< f)

-- Parallel composition of parametrized functions
(.||) :: (_)
    => ParaType p a b
    -> ParaType q c d
    -> ParaType (p, q) (a, c) (b, d)
(Para p f) .|| (Para q g) = Para (p, q) (f .| g)

-------------------------------------------------------------------

data TrainType p a b c = Train {
    _para :: ParaType p a b,
    _cost :: DType b c, -- What constraints does c need to satisfy to measure cost? additive?
    _optimizer :: (p, p) -> p
}
makeLenses ''TrainType

--How should cost functions be composed in the type below?
(.<<<) :: (_)
    => TrainType q b c c2
    -> TrainType p a b c1
    -> TrainType (p, q) a c _
(Train p2 c2 o2) .<<< (Train p1 c1 o1)
    = Train (p2 .<< p1) undefined (o1 `x` o2 . swapParam)

-- Enriched monoidal product, not sure how to express as a class instance in Haskell
(.|||) :: (_)
    => TrainType p a b c1
    -> TrainType q c d c2
    -> TrainType (p, q) (a, c) (b, d) (c1, c2)
(Train p1 c1 o1) .||| (Train p2 c2 o2)
    = Train (p1 .|| p2) (c1 `x` c2) (o1 `x` o2 . swapParam)


-------------------------------------------------------------------


ff :: ParaType p a b -> a -> b
ff (Para p nn) a = f nn (p, a)

-- The let b == .. part is here just because tensor shapes aren't known at compile time
-- this is due to using hTensor
dd :: OnesLike b => ParaType p a b -> a -> (p, a)
dd (Para p nn) a = let b = f nn (p, a)
                    in d nn (p, a) (onesLike b)
