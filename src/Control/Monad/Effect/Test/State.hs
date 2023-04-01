{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Effect.Test.State
    ( simple
    , recursiveCount
    )
where


import Prelude hiding (all)
import Control.Monad.Effects (State(..), modify)
import Control.Monad.Effect.Test.Internal



simple 
    :: (Eq s, State s m) 
    => (s -> s)  -- ^ Operation on the state.
    -> s         -- ^ Initial value for the state.
    -> s         -- ^ Expected value after operation has been applied to the state.
    -> m Result
simple operate s0 s1 = do
    ()  <- put s0
    g0 <- get
    ()  <- modify operate
    g1 <- get
    case (g0 == s0, g1 == s1) of
        (True, True) -> success
        (False, True) -> failure ""
        (True, False) -> failure ""
        (False, False) -> failure ""



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
