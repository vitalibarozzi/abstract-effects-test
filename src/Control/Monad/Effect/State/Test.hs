{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Effect.State.Test
    ( all
    , simple
    , recursiveCount
    )
where


import Prelude hiding (all)
import Control.Monad.Effects (State(..))
import Control.Monad.Effect.Test.Helpers



all :: (Monad m, State Integer m) => [m Result]
all = 
    [ simple 10
    , recursiveCount 10
    ]



simple :: (Monad m, State Integer m) => Integer -> m Result
simple n = do
    put n
    p <- get
    if p /= n
        then return (Left "...") 
        else do
            put (p+1)
            q <- get
            if q == p+1
                then return (Right ())
                else return (Left "...") 



recursiveCount :: (Monad m, State Integer m) => Integer -> m Result
recursiveCount top = do
    put (0 :: Integer)
    let go = do
          n <- get
          if n >= abs top
              then return (Right ())
              else do
                  put (n + 1)
                  go
    go
