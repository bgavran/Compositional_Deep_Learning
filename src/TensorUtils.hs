module TensorUtils where

import System.Random
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

ds # cs = listArray ds cs :: Array Double

class ArrShow a where
    arrShow :: a -> String

instance (Show a, Num a) => ArrShow a where
    arrShow = show

instance ArrShow (NArray None Double) where
    arrShow = formatFixed 2

instance (ArrShow a, ArrShow b) => ArrShow (a, b) where
    arrShow (a, b) = let ls = replicate 10 '-' ++ "\n"
                     in ls ++ arrShow a ++ ", \n" ++ arrShow b ++ "\n" ++ ls


sh :: ArrShow a => a -> IO ()
sh a = putStr $ arrShow a ++ "\n"

takeRandomIO :: Int -> IO [Double]
takeRandomIO 0 = return []
takeRandomIO n = do
    r  <- randomIO
    rs <- takeRandomIO (n-1)
    return (r:rs)

randArray :: [Int] -> [Char] -> IO (NArray None Double)
randArray xs cs = do
    rs <- takeRandomIO (product xs)
    return $ xs # rs ! cs

class OnesLike a where
    onesLike :: a -> a

instance {-# OVERLAPS #-} Num a => OnesLike a where
    onesLike _ = 1

instance {-# OVERLAPS #-} OnesLike (NArray None Double) where
    -- Assumes single-letter index names
    onesLike c = let d  = map iDim $ dims c
                     ch = concatMap iName $ dims c
                 in d # repeat 1 ! ch
