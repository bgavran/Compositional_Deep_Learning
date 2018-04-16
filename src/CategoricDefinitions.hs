{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module CategoricDefinitions where
import GHC.Exts (Constraint)

class Category k where
  type Obj k a b :: Constraint
  id  :: a `k` a
  (.) :: (b `k` c) -> (a `k` b) -> (a `k` c)

class Category k => Monoidal k where
  x :: (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d)) 

class Monoidal k => Cartesian k where
  exl :: (a, b) `k` a
  exr :: (a, b) `k` b
  dup :: a `k` (a, a)

-- Error in the paper, in the paper cocartesian class requires just category and not monoidal?

class Monoidal k => Cocartesian k where
  inl :: a `k` (a, b)
  inr :: b `k` (a, b)
  jam :: (a, a) `k` a

