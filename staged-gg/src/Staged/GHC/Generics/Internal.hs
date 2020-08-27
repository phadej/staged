{-# LANGUAGE TemplateHaskellQuotes #-}
module Staged.GHC.Generics.Internal (sapply) where

import Language.Haskell.TH (Code, Quote)

sapply :: Quote q => Code q (a -> b) -> Code q a -> Code q b
sapply cf cx = [|| $$cf $$cx ||]
