{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Staged.Kleene (
    compileRE,
) where

import Kleene.RE
import Kleene.Internal.Partition
import Data.Traversable (for)
import Staged.Commons

import qualified Data.Set as Set
import qualified Data.Map as Map

compileRE :: Quote q => RE Char -> Code q ([Char] -> Bool)
compileRE re = sletrec aux re

aux :: forall q m. (Quote q, Monad m)
    => (RE Char -> m (Code q ([Char] -> Bool)))
    -> (RE Char -> m (Code q ([Char] -> Bool)))
aux _   re
    | isEmpty re
    = return $ fromFn $ \_ -> sfalse
aux rec re = do
    next' <- fmap Map.fromList $ for (Set.toList $ examples lc) $ \c -> do
        re' <- rec (derivate c re)
        return (c, re')

    let next :: Char -> Code q [Char] -> Code q Bool
        next c xs = case Map.lookup c next' of
            Nothing -> sfalse
            Just k  -> k @@ xs

    return $ fromFn $ \input -> scaseList input
        (if nullable re then strue else sfalse)
        $ \x xs -> partitionCase lc (\c -> next c xs) x
  where
    lc = leadingChars re

partitionCase
    :: forall q r. Quote q
    => Partition Char
    -> (Char -> Code q r)
    -> (Code q Char -> Code q r)
partitionCase p k c = go (Set.toList $ examples p) where
    go :: [Char] -> Code q r
    go []  = error "empty examples"
    go [x] = k x
    go (x:xs) = sIfThenElse (toCode [|| $$(fromCode c) <= x ||])
        (k x)
        (go xs)
