module Para where

import Prelude hiding (id, (.))

import CategoricDefinitions
import Autodiff.GAD
import Autodiff.Additive
import Autodiff.Dual
import Autodiff.DType

----------------------------------

data PType p
    = P p
    | (PType p) `X` (PType p)
    deriving (Eq, Show, Functor)

instance {-# OVERLAPS #-} Additive p => Additive (PType p) where
    zero = P zero
    (P p1) ^+ (P p2) = P (p1 ^+ p2)
    (p1 `X` q1) ^+ (p2 `X` q2) = (p1 ^+ p2) `X` (q1 ^+ q2)

toTuple :: PType p -> (PType p, PType p)
toTuple (p `X` q) = (p, q)
toTuple (P p) = error "nope!"

dToTuple :: DType (PType p) (PType p, PType p)
dToTuple = D $ linearD toTuple (Dual (AddFun (uncurry X))) -- duplicate from Comonad?

extractP :: PType p -> p
extractP (P p) = p

dFromP :: DType (PType p) p
dFromP = D $ linearD extractP (Dual (AddFun P))

leftFromP :: Additive2 p a => DType (PType p, a) (p, a)
leftFromP = dFromP `x` id

{-
Swap map for mon. product of parametrized functions, from top to bottom
(a b) (c d)
a (b, (c, d))
a ((b, c), d)
a ((c, b), d)
a (c, (b, d))
(a c) (b d)
-}

swapParam :: (Monoidal k, _) => ((a, b), (c, d)) `k` ((a, c), (b, d))
swapParam = assocR . (id `x` assocL) . (id `x` (swap `x` id)) . (id `x` assocR) . assocL

----------------------------------


newtype ParaType p a b = Para {
    evalPara :: DType (PType p, a) b
}

instance Category (ParaType p) where
    type Allowed (ParaType p) a = Allowed2 DType (PType p) a

    id = Para exr
    (Para g) . (Para f) = Para $ (g .< f) . (dToTuple `x` id)


instance Monoidal (ParaType p) where
    (Para f) `x` (Para g) = Para $ (f `x` g) . swapParam . (dToTuple `x` id)
    assocL = Para $ exr . (id `x` assocL) -- also possible: (unitorL . (counit `x` assocR)), which is better?
    assocR = Para $ exr . (id `x` assocR)
    swap = Para $ exr . (id `x` swap)

fp :: ParaType p a b -> (PType p, a) -> b
fp = fGAD . evalDType . evalPara

dfp :: ParaType p a b -> (PType p, a) -> b -> (PType p, a)
dfp para inp = evalGrad $ (dfGAD . evalDType . evalPara) para inp
