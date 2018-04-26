import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

ds # cs = listArray ds cs :: Array Double

sh x = putStr . formatFixed 2 $ x

a = [2, 3] # [1..] ! "ij"
b = [3, 5] # [10..] ! "jk"

c = a * b

-- Assumes single-letter index names
onesLike c = let d  = map iDim $ dims c
                 ch = concat $ map iName $ dims c
             in d # (repeat 1) ! ch

aGrad = (onesLike c) * b
bGrad = (onesLike c) * a
