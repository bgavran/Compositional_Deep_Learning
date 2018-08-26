{-# LANGUAGE 
             EmptyCase,
             FlexibleInstances,
             FlexibleContexts,
             InstanceSigs,
             MultiParamTypeClasses,
             PartialTypeSignatures,
             LambdaCase,
             MultiWayIf,
             NamedFieldPuns,
             TupleSections,
             DeriveFunctor,
             TypeOperators,
             ScopedTypeVariables,
             ConstraintKinds,
             RankNTypes,
             NoMonomorphismRestriction,
             TypeFamilies,
             UndecidableInstances,
             GeneralizedNewtypeDeriving
                            #-}

module AD where

import Prelude hiding (id, (.), curry, uncurry)
import qualified Prelude as P
--import Numeric.LinearAlgebra.Array
--import Numeric.LinearAlgebra.Array.Util

import CategoricDefinitions
import Additive
import Dual
import Cont

newtype DType a b = D {
  evalD :: a -> (b, DType a b)
}

-- DType a -> b can be nonlinear, but it's derivative can't be!
-- Is this type of recursive structure needed?
-- If the derivative is a linear map, then how can we take its derivative?
-- Something is off here? 
-- Because there's a difference between taking a derivative of a linear map and taking a derivative of a nonlinear function whose derivative is a linear map?
-- In the first case, we get the same linear map after differentiation. In the second case, we shouldn't! What do we get?

linearD :: (x -> y) -> DType x y
linearD f = D $ \x -> (f x, linearD f)

instance Category DType where
  type Allowed DType x = Additive x
  id      = linearD id
  D g . D f   = D $ \a -> let (b, f') = f a
                              (c, g') = g b
                          in (c, g' . f')

instance Monoidal DType where
  D f `x` D g = D $ \(a, b) -> let (c, f') = f a
                                   (d, g') = g b
                               in ((c, d), f' `x` g')

instance Cartesian DType where
  exl = linearD exl
  exr = linearD exr
  dup = linearD dup

instance Cocartesian DType where
  inl = linearD inlF
  inr = linearD inrF
  jam = linearD jamF

instance Closed DType where
  apply :: (DType a b, a) -> b
  apply (D op, a) = fst $ op a

  curry :: Additive3 a b c => DType (a, b) c -> DType a (DType b c)
  curry d = D $ \a -> let dbc = pd d a -- DType b c
                      in (dbc, D $ \da -> (   D $ \db -> (apply (df dbc db, db), undefined)     , undefined))
--                            DType a (DType b c)                       ^ this should be b and not db!

--  curry d@(D op) = D $ \a -> let dbc = D $ \b -> let (c, op') = op (a, b) -- op' :: DType (a, b) c
--                                                 in (c, apply (curry op', a)) -- this is wrong! we're applying a on (curry op'), but we should be applying da!
--                             in (dbc, undefined)
--                           D $ \da -> (D $ \db -> let (_, D op') = op (a, b)
--                                                               (dc, op'') = op' (da, db)
--                                                           in (dc, apply (curry op'', da)), undefined))
--                                                              DType b c    DType a (DType b c)
  uncurry :: DType a (DType b c) -> DType (a, b) c
  uncurry (D op) = D $ \(a, b) -> let (D bc, op') = op a 
                                      (c, bc')    = bc b -- bc' :: DType b c - but we don't need bc?
                                  in (c, uncurry op')

-- WORK out the dynamics of this?
-- partially apply stuff on D. Output c depends both on a and b, while the derivative is just a function of B
-- in unjoin, the (DType b c)'s output is only a function of b. (for argument 'a' the unit element was passed)
pd :: Additive3 a b c => DType (a, b) c -> a -> DType b c
pd (D op) a = D $ \b -> let (c, op') = op (a, b)
                        in (c, op' . inr)

newtype ScaleWrapper a d = SW {
  evalSW :: (a, d)
} 

class ScalableNew k a b c where
  scaleNew :: a -> (b `k` c)

instance Num a => ScalableNew (DType) a (DType b c) (DType b c) where
  scaleNew a = linearD id

instance Num a => ScalableNew (DType) a a a where
  scaleNew k = D $ \a -> (a*k, constD k)

constD :: Num b => b -> DType a b
constD = \k -> D $ \_ -> (k, constD 0)


-- DType also needs to have a Num instance?
-- currying implies DType a b can also be an argument to DType itself.
-- It means they're all objects in the same category.
-- That means I have to generalize the concept of scaling, but only provide the runnable instance for the actual scalable stuff.?


-- (DType b c) just needs to be an instance of scalable? I need to be able to "multiply" a function, whatever that means.
--
-- difference between 
-- apply (curry op', zero)
-- and
-- op' . inr? 

unjoinD :: Additive3 a b c => DType (a, b) c -> (DType a c, DType b c)
unjoinD = unjoin

dtest :: DType (Double, Double) Double
dtest = D $ \(a, b) -> (a + 2*a*b + exp b, D $ \(a, b) -> (1 + 2*b + 2*a + exp b, undefined))

dtestCurry :: DType Double (DType Double Double)
dtestCurry = D $ \a -> (D $ \b -> (a + 2*a*b + exp b, D $ \dy -> (2*a + exp dy, undefined)), gg)

gg :: DType Double (DType Double Double)
gg = D $ \a -> (D $ \b -> (1 + 2*b + 2*a + exp b, undefined), undefined)

applydf :: Additive a => DType a b -> a -> b
applydf d a = apply (df d a, one)

curapply :: DType (a, b) c -> a -> DType b c
curapply (D op) = \a -> D $ \b -> let (c, op') = op (a, b)
                                  in (c, curapply op' a)

mul :: (Num a, Additive a) => DType (a, a) a
mul = D $ \(a, b) -> (a * b, curapply mul b \/ curapply mul a) 

--sigm :: (Additive a, Floating a) => DType a a
--sigm = D $ \a -> let s = 1 / (1 + exp (-a))
--                 in (s, as)
--
--
--subFrom1 :: (Num a, Additive a) => DType a a
--subFrom1 = (curapply jam 1) . (curapply mul (-1))
--
--dfs :: (Num a, Additive a) => DType b (DType a a, b)
--dfs = linearD $ \s -> (mul . (id /\ subFrom1), s)
--
--as = apply . dfs

df :: DType a b -> a -> DType a b
df (D op) = snd . op

instance {-# OVERLAPS #-} Additive a => Additive (DType a a) where
-- does this instance even make sense? Perhaps just zero is needed and it's not "additive"
  zero = id
  one = undefined
  (^+) = undefined

