{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Effect.Test.Random
where


import Prelude hiding (all)
import Control.Monad.Effects (Random(..))
import Control.Monad.Effect.Test.Internal


nonEqAfter 
    :: (Random Integer m) 
    => Int
    -> m Result
nonEqAfter max_ = do
    let go n = 
            if n >= max_
                then do
                    a <- random @Integer
                    b <- random @Integer
                    if a == b
                        then failure ""
                        else success
                else do
                    a <- random @Integer
                    b <- random @Integer
                    if a == b
                        then failure ""
                        else go (n + 1)
    go 0
