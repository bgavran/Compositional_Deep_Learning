module Para where

import Prelude hiding (id, (.))

import CategoricDefinitions
import GAD
import Dual
import Additive


----------------------------------

newtype DType a b = D {
  evalDType :: GADType (DualType (->+)) a b
} deriving (Category, Monoidal, Cartesian, Cocartesian)

instance Num s => NumCat DType s where
  negateC = D negateC
  addC = D addC
  mulC = D mulC
  increaseC a = D $ increaseC a

instance Floating s => FloatCat DType s where
  expC = D expC

instance Fractional s => FractCat DType s where
  recipC = D recipC

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

dToP :: DType a (PType a)
dToP = D $ linearD P (Dual (AddFun extractP)) -- extract from Comonad?

leftToP :: Additive2 p a => DType (p, a) (PType p, a)
leftToP = dToP `x` id

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
swapParam = assocR . (id `x` assocL) . (id `x` (swap `x` id)) . (id `x` assocR) .  assocL

comp :: (_) => (q, b) `k` c -> (p, a) `k` b -> ((p, q), a) `k` c
g `comp` f = g . (id `x` f) . assocL . (swap `x` id)

----------------------------------

newtype ParaType p a b = Para {
  evalPara :: DType (PType p, a) b
}

instance Category (ParaType p) where
  type Allowed (ParaType p) a = Allowed2 DType (PType p) a

  id = Para exr
  (Para g) . (Para f) = Para $ (g `comp` f) . (dToTuple `x` id)


instance Monoidal (ParaType p) where
  (Para f) `x` (Para g) = Para $ (f `x` g) . swapParam . (dToTuple `x` id)
  assocL = Para $ exr . (id `x` assocL) -- also possible: (unitorL . (counit `x` assocR)), which is better?
  assocR = Para $ exr . (id `x` assocR)
  swap = Para $ exr . (id `x` swap)



{-
-- this is partially defined?
instance Comonad Z where
  extract (P p) = p
  duplicate (P p) = P (P p)

dFunctor :: Additive3 p a b => DType a b -> DType (p, a) (p, b)
dFunctor (GAD f) = GAD $ \(p, a) -> let (b, f') = f a
                                    in ((p, b), id `x` f')

delt :: DType (Z p, a) (Z p, (Z p, a))
delt = linearD $ \(p `X` q, a) -> (p, (q, a))

comp :: Additive4 (Z p) a b c => DType (Z p, b) c -> DType (Z p, a) b -> DType (Z p, a) c
g `comp` f = g . (dFunctor f) . delt
-}
