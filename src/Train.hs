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
import TensorUtils

-------------------------------------------

data ParaType p a b = Para {
    _param :: p,
    _fn :: DType (p, a) b
}
makeLenses ''ParaType

-- Compose two parametrized functions
(.<<) :: (_)
    => ParaType q b c
    -> ParaType p a b
    -> ParaType (p, q) a c
(Para q g) .<< (Para p f) = Para (p, q) (g .< f)

fp :: ParaType p a b -> a -> b
fp (Para p nn) a = f nn (p, a)

-- The let b == .. part is here just because tensor shapes aren't known at compile time
-- this is due to using hTensor
dfp :: OnesLike b => ParaType p a b -> a -> (p, a)
dfp (Para p nn) a = let b = f nn (p, a)
                    in df nn (p, a) (onesLike b)

data TrainType p a b c = Train {
    _para :: ParaType p a b,
    _cost :: DType b c, -- What constraints does c need to satisfy to measure cost? additive?
    _optimizer :: (p, p) -> p
}
makeLenses ''TrainType

sgd :: Fractional p => (p, p) -> p
sgd (p, pGrad) = p - 0.1 * pGrad

extend :: (_) => ParaType p a b -> DType b c -> ParaType p a c
extend para cost = para & fn %~ (cost.)

train :: (OnesLike c, _) => TrainType p a b c -> a -> p
train (Train para cost optimizer) a = let nnWithCost = extend para cost
                                          (pGrad, _) = dfp nnWithCost a
                                      in optimizer (para ^. param, pGrad)

trainStep :: (OnesLike c, _) => TrainType p a b c -> a -> TrainType p a b c
trainStep nn a = nn & para . param .~ train nn a

-- need static tensors!
randomInitialParam :: DType (Double, a) b -> IO (ParaType Double a b)
randomInitialParam fd = do
    p <- randomIO
    return $ Para (10*p) fd

sampleData :: IO Double
sampleData = fmap (*10) randomIO

showNNInfo :: (Show p, Show a) => Int -> a -> TrainType p a b c -> IO ()
showNNInfo n a nn = do
    putStrLn "-------------------------"
    putStrLn $ "Step " ++ show n
    putStrLn $ "p == " ++ show (nn ^. para . param)
    putStrLn $ "a == " ++ show a

trainNSteps :: (_) => Int -> IO a -> TrainType p a b c -> IO (TrainType p a b c)
trainNSteps 0 sampler nn = return nn
trainNSteps n sampler nn = do
    a <- sampler
    when (n `mod` 100 == 0) $ showNNInfo n a nn
    trainNSteps (n - 1) sampler $ trainStep nn a

mt :: IO ()
mt = do
    l1 <- randomInitialParam (sigmoid . mulC)
    let nn = Train {
            _para = l1,
            _cost = id,
            _optimizer = sgd}

    trainNSteps 1000 sampleData nn

    return ()
