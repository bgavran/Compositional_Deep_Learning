module Train where

import Control.Lens hiding ((#), para)
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

sgd :: Fractional p => (p, p) -> p
sgd (p, pGrad) = p - 0.1 * pGrad

-- Extends a parametrized differentiable function with another differentiable function
-- Useful for when we want to compose some Para with a cost function
extendPara :: _ => ParaType p a b -> DType b c -> ParaType p a c
extendPara para cost = para & fn %~ (cost.)

train :: (OnesLike c, _) => LearnerType p a b c -> a -> p
train (Learner para cost optimizer) a = let nnWithCost = extendPara para cost
                                            (pGrad, _) = dd nnWithCost a
                                        in optimizer (para ^. param, pGrad)

trainStep :: (OnesLike c, _) => LearnerType p a b c -> a -> LearnerType p a b c
trainStep nn a = nn & para . param .~ train nn a

-- need static tensors!
randomInitialParam :: DType (Double, a) b -> IO (ParaType Double a b)
randomInitialParam fd = do
    p <- randomIO
    return $ Para (10*p) fd

sampleData :: IO Double
sampleData = fmap (*10) randomIO

showNNInfo :: (Show p, Show a) => Int -> a -> LearnerType p a b c -> IO ()
showNNInfo n a nn = do
    putStrLn "-------------------------"
    putStrLn $ "Step " ++ show n
    putStrLn $ "p == " ++ show (nn ^. para . param)
    putStrLn $ "a == " ++ show a

trainNSteps :: (_) => Int -> IO a -> LearnerType p a b c -> IO (LearnerType p a b c)
trainNSteps 0 sampler nn = return nn
trainNSteps n sampler nn = do
    a <- sampler
    when (n `mod` 100 == 0) $ showNNInfo n a nn
    trainNSteps (n - 1) sampler $ trainStep nn a

mt :: IO ()
mt = do
    l1 <- randomInitialParam (sigmoid . mulC)
    let nn = Learner {
            _para = l1,
            _cost = id,
            _optimizer = sgd}

    trainNSteps 1000 sampleData nn

    return ()
