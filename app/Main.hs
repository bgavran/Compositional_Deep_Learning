module Main where

import Prelude hiding (id, (.))
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util
import System.Random
import Control.Monad
import Control.Lens hiding ((#), para)

import CategoricDefinitions
import Autodiff.DType

main :: IO ()
main = print "Hello, world!"
