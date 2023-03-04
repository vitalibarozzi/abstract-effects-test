{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Effect.Test.Random
    ( nonRepeating
    , sparseRepeating
    )
where


import Prelude hiding (all)
import Control.Monad.Effects (Random(..))
import Control.Monad.Effect.Test.Helpers


nonRepeating :: forall a m proxy. (Eq a, Monad m, Random a m) => proxy a -> m Result
nonRepeating _ = do
    (a0, a1) <- roll
    if a0 /= (a1 :: a) 
        then success 
        else failure "the generated values are the same"


sparseRepeating :: forall a m proxy. (Eq a, Monad m, Random a m) => Int -> proxy a -> m Result
sparseRepeating factor _ = do
    if factor >= 0
        then go factor
        else failure "factor need to be bigger than 0"
  where
    go n = do
        if n <= 0
            then failure "too many repeating values"
            else do
                (a0, a1) <- roll
                if a0 /= (a1 :: a)
                    then success 
                    else go (n-1)
