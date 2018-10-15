module TrainUtils where

import Control.Lens hiding (para)
import System.Random
import Control.Monad
import Prelude hiding (id, (.))
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

import CategoricDefinitions
import Autodiff.GAD
import Autodiff.Dual
import Autodiff.Additive
import Autodiff.DType
import Ops
import Para
import TensorUtils

-------------------------------------------

-- Extends a parametrized differentiable function with another differentiable function
-- Useful for when we want to compose some Para with a cost function
extendPara :: _ => ParaType p a b -> DType b c -> ParaType p a c
extendPara para cost = para & fn %~ (cost.)

calcNewParam :: (OnesLike c, _) => LearnerType p a b c -> a -> p
calcNewParam (Learner p para cost optimizer) a = let nnWithCost = extendPara para cost
                                                     (pGrad, _) = dd nnWithCost (p, a)
                                                 in optimizer (p, pGrad)

showNNInfo :: (ArrShow p, ArrShow a) => Int -> a -> LearnerType p a b c -> IO ()
showNNInfo n a nn = do
    putStrLn "-------------------------"
    putStrLn $ "Step " ++ arrShow n
    putStrLn $ "p\n" ++ arrShow (nn ^. p)
    putStrLn $ "a\n" ++ arrShow a


trainStep :: (OnesLike c, _)
    => LearnerType p a b c -> (Int, IO a) -> IO (LearnerType p a b c)
trainStep nn (step, dataSampler) = do
    a <- dataSampler
    when (step `mod` 100 == 0) $ showNNInfo step a nn
    return $ nn & p .~ calcNewParam nn a
