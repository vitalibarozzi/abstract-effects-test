{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Effect.Test.State
    ( simple
    , recursiveCount
    )
where


import Prelude hiding (all)
import Control.Monad.Effects (State(..))
import Control.Monad.Effect.Test.Helpers



simple 
    :: (Monad m, State s m) 
    => (s -> s)  -- ^ Operation on the state.
    -> s         -- ^ Initial value for the state.
    -> s         -- ^ Expected value after operation has been applied to the state.
    -> m Result
simple operate s0 s1 = do
    undefined
    {-
    _  <- put s0
    g0 <- get
    _  <- modify operate
    g1 <- get
    case (g0 == s0, g1 == s1) of
        (True, True) -> sucess
        (False, True) -> failure "(get . put) failed."
        (True, False) -> failure "(get . modify) failed."
        (False, False) -> failure "both (get . modify) and (get . put) have failed."
-}



recursiveCount :: (Monad m, State Integer m) => Integer -> m Result
recursiveCount top = do
    undefined
{-
    put (0 :: Integer)
    let go = do
          n <- get
          if n >= abs top
              then return (Right ())
              else do
                  put (n + 1)
                  go
    go
-}
