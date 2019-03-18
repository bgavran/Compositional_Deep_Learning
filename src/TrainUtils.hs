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
import Autodiff.D
import Autodiff.Cont
import Ops
import Para
import TensorUtils
import OnesLike

-------------------------------------------
showNNInfo :: (ArrShow p, ArrShow a, ArrShow b)
    => Int -> a -> b -> LearnerType p a b -> IO ()
showNNInfo n a b nn = do
    putStrLn "-------------------------"
    putStrLn $ "Step " ++ arrShow n
    putStrLn $ "p\n" ++ arrShow (nn ^. p)
    putStrLn $ "a\n" ++ arrShow a
    putStrLn $ "a\n" ++ arrShow b


-- | Supervised learning training
-- Takes in a Learner, input-output pairs and a cost function
-- it partially applies the output to the cost function and composes the result inside learner
trainStepWithCost :: (OnesLike c, _)
    => LearnerType p a b -> (Int, IO (a, b), DType (b, b) c) -> IO (LearnerType p a b)
trainStepWithCost l (step, dataSampler, cost) = do
    (i, o) <- dataSampler
    when (step `mod` 100 == 0) $ showNNInfo step i o l
    let cost' = partiallyApply cost o
        (pGrad, _) = grad (cost' . (l ^. para . fn)) (l ^. p, i)
    return $ l & p .~ (l ^. optimizer) (l ^. p, pGrad)


instance (Random a, Random b) => Random (a, b) where
    random gen1 = let (x, gen2) = random gen1
                      (y, gen3) = random gen2
                  in ((x, y), gen3)

    randomR ((x1, y1), (x2, y2)) gen1 = let (x, gen2) = randomR (x1, x2) gen1
                                            (y, gen3) = randomR (y1, y2) gen2
                                        in ((x, y), gen3)
