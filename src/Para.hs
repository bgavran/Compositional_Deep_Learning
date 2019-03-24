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
import AsymmetricLens

-------------------------------------------------------------------

newtype ParaType (k :: Type -> Type -> Type) p a b = Para {
    _fn :: (p, a) `k` b
}
makeLenses ''ParaType

instance Monoidal k => Category' (ParaType k) where
    type Allowed' (ParaType k) a = (Allowed k a)
    (.*) = Para unitorL
    (Para g)  .-  (Para f) = Para (g .-- f)

instance Monoidal k => Monoidal' (ParaType k) where
    (Para f) .| (Para g) = Para (f .|| g)

-------------------------------------------------------------------

type ParaDType = ParaType DType

-- type ParaLType = ParaType (AsymmetricLens DType)

data LearnerType p a b = Learner {
    _p :: p,
    _para :: ParaDType p a b,
    _optimizer :: (p, p) -> p
}
makeLenses ''LearnerType

instance Category' LearnerType where
    type Allowed' LearnerType a = Allowed' ParaDType a

    (.*) = Learner () (.*) snd
    (Learner p2 f2 o2) .- (Learner p1 f1 o1) =
        Learner (p1, p2) (f2 .- f1) (o1 `x` o2 . swapParam)

instance Monoidal' LearnerType where
    (Learner p1 f1 o1) .| (Learner p2 f2 o2)
        = Learner (p1, p2) (f1 .| f2)  (o1 `x` o2 . swapParam)
