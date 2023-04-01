{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Effect.Test.StatePartial
where


import Prelude hiding (all)
import Control.Monad.Effects (State(..), modify, nil, Partial(..), catches, Recover(..), Void)
import Control.Monad.Effect.Test.Internal


nonTransitional 
    :: ( State Integer m
       , Partial m
       , Recover m
       )
    => m Result
nonTransitional = do
    put (256 :: Integer)
    catches handleErr do
        s <- get
        put (s + 256)
        nil
    s <- get
    if s == 512
        then success
        else failure ""
  where
    handleErr = const (return ())


transitional 
    :: ( State Integer m
       , Partial m
       , Recover m
       )
    => m Result
transitional = do
    put (256 :: Integer)
    catches handleErr do
        s <- get
        put (s + 256)
        nil
    s <- get
    if s == 256
        then success
        else failure ""
  where
    handleErr = const (return ())
