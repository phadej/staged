{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Staged.Stream.Fallible.Step (
    FallibleStep (..),
    ) where

-- | Step of state machine
data FallibleStep a s
    = Stop       -- ^ stop
    | Skip s     -- ^ go to next step
    | Emit a s   -- ^ go to next stop producing output.
    | Fail       -- ^ fail
  deriving (Functor, Foldable, Traversable)
