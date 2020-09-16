module Kleene.RE.NonEps (
    NERE (..),
    nere,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe         (mapMaybe)
import Data.RangeSet.Map  (RSet)
import Data.Set           (Set)
import Kleene.RE          (RE (..))

import qualified Data.RangeSet.Map as RSet
import qualified Data.Set          as Set

-- | Non-eps 'RE' variant.
data NERE c
    = NEREChars (RSet c)
    | NEREAppend (NonEmpty (NERE c))
    | NEREUnion (RSet c) (Set (NERE c))
    | NEREPlus (NERE c)
 deriving (Eq, Ord, Show)

-- | Extract non-eps part of 'RE'.
nere :: Ord c => RE c -> Maybe (NERE c)
nere (REChars r) = Just (NEREChars r)
nere (REUnion r xs)
    | RSet.null r
    , Set.null ys
    = Nothing

    | otherwise
    = Just (NEREUnion r ys)
  where
    ys = Set.fromList . mapMaybe nere . Set.toList $ xs

nere (REAppend xs) = case mapMaybe nere xs of
    []   -> Nothing
    y:ys -> Just (NEREAppend (y :| ys))

nere (REStar r) = fmap NEREPlus (nere r)
