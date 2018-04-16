{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module CategoricDefinitions where

import GHC.Exts (Constraint)

class Category k where
  id  :: a `k` a
  (.) :: (b `k` c) -> (a `k` b) -> (a `k` c)

class Category k => Monoidal k where
  x :: (a `k` c) -> (b `k` d) -> ((a, b) `k` (c, d)) 

class Monoidal k => Cartesian k where
  exl :: (a, b) `k` a
  exr :: (a, b) `k` b
  dup :: a `k` (a, a)

-- I haven't figured out yet how ConstraintKinds work so that's why there's a Num constraint here and throughout many places in the code. 

class Category k => Cocartesian k where
  inl :: (Num a, Num b) => a `k` (a, b)
  inr :: (Num a, Num b) => b `k` (a, b)
  jam :: Num a => (a, a) `k` a

