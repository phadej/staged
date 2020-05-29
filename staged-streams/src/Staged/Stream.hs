-- | Monadic staged streams.
module Staged.Stream (
    -- * Stream
    StreamM (..),
    Step (..),
    mkStreamM,
    -- * Combinators
    --
    -- | See "Staged.Stream.Pure" for introduction to combinators.
    --   Remember that monadic 'StreamM' is like @a -> ListT m b@ function,
    --   and not for @a -> m [b]@ or @a -> [m b]@.
    module Staged.Stream.Combinators,
    ) where

import Staged.Stream.Type
import Staged.Stream.Convenience
import Staged.Stream.Combinators
