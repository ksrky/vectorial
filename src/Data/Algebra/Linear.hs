{-# LANGUAGE LinearTypes #-}

module Data.Algebra.Linear where

import Data.Num.Linear

class (Ring a, AdditiveGroup v) => Module a v where
    (*>) :: a %1 -> v %1 -> v
