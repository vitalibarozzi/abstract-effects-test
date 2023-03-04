module Control.Monad.Effect.Test.Helpers
    ( Result
    , success
    , failure
    )
where


import Data.Text (Text)


type Result = Either Text ()


success :: (Monad m) => m Result
success = return (Right ())


failure :: (Monad m) => Text -> m Result
failure = return . Left
