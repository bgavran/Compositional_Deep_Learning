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

newtype ParaType p a b = Para {
    _fn :: DType (p, a) b
}
makeLenses ''ParaType

-- Sequential composition of parametrized functions
(.<<) :: (_)
    => ParaType q b c
    -> ParaType p a b
    -> ParaType (p, q) a c
(Para g) .<< (Para f) = Para (g .< f)

-- Parallel composition of parametrized functions
(.||) :: (_)
    => ParaType p a b
    -> ParaType q c d
    -> ParaType (p, q) (a, c) (b, d)
(Para f) .|| (Para g) = Para (f .| g)

-------------------------------------------------------------------

data LearnerType p a b c = Learner {
    _p :: p,
    _para :: ParaType p a b,
    _cost :: DType b c,
    _optimizer :: (p, p) -> p
}
makeLenses ''LearnerType

-- Sequential composition of Learners
-- Paras are composed sequentially,
-- Optimizers are composed in parallel
-- What should be the cost function?
(.<<<) :: (_)
    => LearnerType q b c c2
    -> LearnerType p a b c1
    -> LearnerType (p, q) a c _
(Learner p2 f2 c2 o2) .<<< (Learner p1 f1 c1 o1)
    = Learner (p1, p2) (f2 .<< f1) undefined (o1 `x` o2 . swapParam)

-- Parallel composition of Learners
-- Everything is composed in parallel
(.|||) :: (_)
    => LearnerType p a b c1
    -> LearnerType q c d c2
    -> LearnerType (p, q) (a, c) (b, d) (c1, c2)
(Learner p1 f1 c1 o1) .||| (Learner p2 f2 c2 o2)
    = Learner (p1, p2) (f1 .|| f2) (c1 `x` c2) (o1 `x` o2 . swapParam)


-------------------------------------------------------------------


ff :: ParaType p a b -> (p, a) -> b
ff (Para nn) (p, a) = f nn (p, a)

-- The let b == .. part is here just because tensor shapes aren't known at compile time
-- this is due to using hTensor
dd :: OnesLike b => ParaType p a b -> (p, a) -> (p, a)
dd (Para nn) (p, a) = let b = f nn (p, a)
                      in d nn (p, a) (onesLike b)
