-- |
--
-- @staged-gg@ is a staged version of "GHC.Generics".
-- The abstraction overhead of "GHC.Generics" is removed.
--
-- See https://www.andres-loeh.de/StagedSOP/ paper for description of
-- @staged-sop@, which is staged version of @generics-sop@.
-- The non generics-sop specific parts
-- like sections on Typed Template Haskell, and Type Template Haskell and Constraints,
-- are applicable to @staged-gg@ as well.
--
-- == Examples
--
-- See "Staged.GHC.Generics.Examples" module for examples.
--
-- == Differences from "GHC.Generics"
--
-- * @staged-gg@ is staged: we have 'Code' in the leaves of used representation.
--
-- * Representation 'Rep' class is additionally parametrised by a type-constructor @q@,
--   which in @staged-gg@ is instantiated to @Code q@.
--
-- * There is no @Rec1@ analogue, we use 'K2', which doesn't have an extra index.
--
-- * There is no @:.:@ and @Rec1@. Instead we use left associative applications of ':@@:' headed with 'Par2'.
--   These changes are discussed in GHC issues
--   [https://gitlab.haskell.org/ghc/ghc/-/issues/15969](#15969) and
--   [https://gitlab.haskell.org/ghc/ghc/-/issues/7492](#7492).
--
-- How 'GHC.Generics.Rep' is different is also described by 'Translate' type family.
--
module Staged.GHC.Generics (
    -- * Generic representation types
    V2,
    U2 (..),
    M2 (..),
    K2 (..),
    Par2 (..),
    (:++:) (..),
    (:**:) (..),
    (:@@:) (..),
    -- * Synonyms for convenience
    D2, C2, S2,
    D, C, S,
    -- * Meta-information
    Datatype (..),
    Constructor (..),
    Selector (..),
    Fixity (..),
    FixityI (..),
    Associativity (..),
    SourceUnpackedness (..),
    SourceStrictness (..),
    DecidedStrictness (..),
    Meta (..),
    -- * Generic type classes
    Generic (..),
    Generic1 (..),
    -- * TH Types
    Code, Quote,
    -- * Utilities
    Translate,
    -- * TH deriving machinery
    deriveGeneric,
    deriveGeneric1,
) where

import Staged.GHC.Generics.Instances ()
import Staged.GHC.Generics.TH
import Staged.GHC.Generics.Types
