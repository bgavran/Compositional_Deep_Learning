module Para where

import Prelude hiding (id, (.))
import Control.Lens hiding ((#), para)
import GHC.Exts (Constraint)
import Data.Kind (Type)

import CategoricDefinitions
import Autodiff.GAD
import Autodiff.Additive
import Autodiff.Dual
import Autodiff.D
import TensorUtils
import OnesLike

-------------------------------------------------------------------

newtype ParaType p a b = Para {
    _fn :: DType (p, a) b
}
makeLenses ''ParaType

instance Category' ParaType where
    type Allowed' ParaType a = (Allowed DType a, OnesLike a)

    (.*) = Para exr
    (Para g) .- (Para f) = Para (g .-- f)

instance Monoidal' ParaType where
    (Para f) .| (Para g) = Para (f .|| g)

-------------------------------------------------------------------

data LearnerType p a b = Learner {
    _p :: p,
    _para :: ParaType p a b,
    _optimizer :: (p, p) -> p
}
makeLenses ''LearnerType

instance Category' LearnerType where
    type Allowed' LearnerType a = Allowed' ParaType a

    (.*) = Learner () (.*) snd
    (Learner p2 f2 o2) .- (Learner p1 f1 o1) =
        Learner (p1, p2) (f2 .- f1) (o1 `x` o2 . swapParam)

instance Monoidal' LearnerType where
    (Learner p1 f1 o1) .| (Learner p2 f2 o2)
        = Learner (p1, p2) (f1 .| f2)  (o1 `x` o2 . swapParam)
