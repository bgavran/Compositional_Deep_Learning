module TensorUtils where

import Prelude hiding ((.), id)
import System.Random
import Control.Monad

import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

import CategoricDefinitions
import Autodiff.GAD
import Autodiff.D
import Autodiff.Dual
import Autodiff.Additive

import OnesLike


{-
All tensor stuff is pretty much ad-hoc, a complete rewrite is eventually needed
-}

class ArrShow a where
    arrShow :: a -> String

instance {-# OVERLAPPABLE #-} Show a => ArrShow a where
    arrShow = show

instance  ArrShow Tensor where
    arrShow = formatFixed 2

instance (ArrShow a, ArrShow b) => ArrShow (a, b) where
    arrShow (a, b) = let ls = replicate 10 '-' ++ "\n"
                     in ls ++ arrShow a ++ ", \n" ++ arrShow b ++ "\n" ++ ls


sh :: ArrShow a => a -> IO ()
sh a = putStr $ arrShow a ++ "\n"

infixl 8 −|
--(−|) :: Name → [Array Double ] → Array Double
(−|) = index


axes :: Tensor -> String -> (String, [Int])
axes t axesNames = let toSum = filter (\idx -> head (iName idx) `elem` axesNames) (dims t)
                   in (map (head . iName) toSum, map iDim toSum)


class TensorContractable k where
    sumAxes :: String -> Tensor `k` Tensor

instance TensorContractable (->) where
    sumAxes axesNames = \t -> let (names, lengths) = axes t axesNames
                                  u = lengths # repeat 1 ! names
                              in t*u


instance (Scalable k Tensor, TensorContractable k)
    => TensorContractable (GADType k) where
    sumAxes axesNames = GAD $ \t -> let (names, lengths) = axes t axesNames
                                        u = lengths # repeat 1 ! names
                                    in (t*u, scale u)

instance (TensorContractable k) => TensorContractable (DualType k) where
    sumAxes axesNames = Dual (sumAxes axesNames)

instance TensorContractable DType where
    sumAxes axesNames = D (sumAxes axesNames)

instance TensorContractable (->+) where
    sumAxes axesNames = AddFun (sumAxes axesNames)

--meanAxes :: _ => String -> Tensor -> Tensor
--meanAxes axesNames t = let (names, lengths) = axes t axesNames
--                           n = product lengths
--                           v = scalar . fromIntegral $ n
--                       in divide (sumAxes axesNames t, v)

-- rand array with given shape and axis names
randomTensor :: [Int] -> String -> IO Tensor
randomTensor xs cs = do
    rs <- replicateM (product xs) randomIO
    return $ xs # rs ! cs
