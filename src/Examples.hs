module Examples where

import Control.Lens hiding (para)
import Prelude hiding (id, (.))
import System.Random
import Control.Monad

import CategoricDefinitions
import Autodiff.GAD
import Autodiff.DType
import Ops
import Para
import TrainUtils

{-
f (p, q) (a, b) = (p + q*a, b)
-}
linReg :: _ => ParaType (a, a) (a, a) (a, a)
linReg = Para $ (addC `x` id) . assocR . (id `x` (mulC `x` id)) . (id `x` assocR) . assocL

-- returns (inp, out) pair
sampleData :: IO (Double, Double)
sampleData = do
    x <- randomIO :: IO Double
    return (x, 3 + 7*x)

run :: IO ()
run = do
    p1 <- randomIO :: IO Double
    p2 <- randomIO :: IO Double
    let nn = Learner {
            _p = (p1, p2),
            _para = linReg,
            _cost = sqDiff,
            _optimizer = sgd}
        sampler = zip [0..] (repeat sampleData)

    finalNet <- foldM trainStep nn (take 10000 sampler)
    putStrLn $ "Starting network parameters: " ++ show (p1, p2)
    putStrLn $ "Final network parameters: " ++ show (finalNet ^. p)

    return ()



--sampleDataTensor :: IO (NArray None Double)
--sampleDataTensor = randArray [3, 5] "jk"
--    p1 <- randArray [2, 3] "ij"
