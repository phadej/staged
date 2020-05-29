{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Staged.Stream.Step (
    Step (..),
    ) where

-- | Step of state machine
data Step a s
    = Stop       -- ^ stop
    | Skip s     -- ^ go to next step
    | Emit a s   -- ^ go to next stop producing output.
  deriving (Functor, Foldable, Traversable)
