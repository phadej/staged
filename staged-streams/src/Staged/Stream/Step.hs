{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Staged.Stream.Step (
    Step (..),
    ) where

data Step a s
    = Stop
    | Skip s     -- our finite state automaton might not produce a value
    | Emit a s
  deriving (Functor, Foldable, Traversable)
