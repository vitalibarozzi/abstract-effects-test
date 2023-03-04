{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Effect.Test.StateException
    ( 
    )
where


import Prelude hiding (all)
import Control.Monad.Effects (State(..), Error(..))
import Control.Monad.Effect.Test.Helpers
