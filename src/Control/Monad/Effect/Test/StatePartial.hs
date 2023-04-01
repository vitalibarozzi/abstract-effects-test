{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Effect.Test.StatePartial
where


import Prelude hiding (all)
import Control.Monad.Effects (State(..), Partial(..))
import Control.Monad.Effect.Test.Helpers



{-
-- | This functions tests how does state and partial interact,
-- the two typical behaviours been "state updates are discarded in
-- case of a failure" or "the state updates are not discarded in case
-- of failure". This function does not test for one of the other specifically,
-- that is decided by the catcher function and the expected value at the end.
-- Since the way a monad will catch exception is decided by this monad itself
-- we need to receive the catcher function as arguments to make use of it in the
-- test.
stateUpdateBehaviour
    :: (Monad m, State s m, Partial m)
    => (s -> s) -- ^ Operation function that changes the state.
    -> s        -- ^ The initial state.
    -> s        -- ^ The expected state at the end.
    -> (m x -> m x -> m x) -- | The catcher function we use to wrap the partial.
    -> m Result 
stateUpdateBehaviour operate s0 s1 catchWith = do
    put s0
    -- TODO on the other hand this looks very strange
    catchWith handleErr (modify operate >> nil)
    g1 <- get
    if g1 == s1
        then success
        else failure ""
-}
