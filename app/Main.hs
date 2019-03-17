module Main where

import Prelude hiding (id, (.))
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util
import System.Random
import Control.Monad
import Control.Lens hiding ((#), para)

import CategoricDefinitions
import Autodiff.D
import Para
import Examples

main :: IO ()
main = run
