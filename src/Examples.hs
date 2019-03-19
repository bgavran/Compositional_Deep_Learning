module Examples where

import Control.Lens hiding (para)
import Prelude hiding (id, (.))
import System.Random
import Control.Monad

import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

import CategoricDefinitions
import Autodiff.GAD
import Autodiff.D
import Ops
import Para
import TrainUtils
import TensorUtils
import OnesLike

{-
f (p, q) a = (p + q*a)
-}
linRegFn :: (Additive a, Num a) => DType ((a, a), a) a
linRegFn = addC . (id `x` mulC) . assocL

-- returns (inp, out) pair we want to learn
sampleData :: IO (Double, Double)
sampleData = do
    x <- randomIO :: IO Double
    return (x, 3 + 7*x)

run :: IO ()
run = do
    initialParams <- randomIO :: IO (Double, Double)
    let initialLearner = Learner initialParams (Para linRegFn) sgd
        sampler = zip3 [0..] (repeat sampleData) (repeat sqDiff)

    finalLearner <- foldM trainStepWithCost initialLearner (take 10000 sampler)
    putStrLn $ "Starting network parameters:\n" ++ show initialParams
    putStrLn $ "Final network parameters:\n" ++ show (finalLearner ^. p)

    return ()


sampleDataTensor :: IO (Tensor, Tensor)
sampleDataTensor = do
    d <- randomTensor [5, 3] "bf"
    return (d, 3 + 7 * d)

-- multiply two arrays you get and sum the "b" axis
l :: _ => ParaDType (Tensor, Tensor) Tensor Tensor
l = Para $ sumAxes "b" . linRegFn

run1 :: IO _
run1 = do
    p1 <- randomTensor [3, 2] "fo"
    p2 <- randomTensor [2] "o"
    let initialLearner = Learner (p1, p2) l sgd
        sampler = zip3 [0..] (repeat sampleDataTensor) (repeat (sumAxes "b" . sqDiff))


    finalLearner <- foldM trainStepWithCost initialLearner (take 1 sampler)
    putStrLn $ "Starting network parameters:\n" ++ arrShow (p1, p2)
    putStrLn $ "Final network parameters:\n" ++ arrShow (finalLearner ^. p)

    return finalLearner
