{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Main (main) where

import Control.Category  ((>>>))
import Control.Exception (SomeException, try)

import qualified Control.Monad.Trans.Resource as R
import qualified Data.ByteString              as BS

import           Staged.Commons
import qualified Staged.Stream            as S
import qualified Staged.Stream.Directory  as S
import qualified Staged.Stream.ByteString as S

main :: IO ()
main = staged

retry :: IO a -> IO a
retry io = do
    ma <- try io
    case ma of
        Right a -> return a
        Left (_ :: SomeException) -> retry io

staged :: IO ()
staged = do
    putStrLn "File stats using 'staged'"

    x <- retry $ R.runResourceT $$(
            S.foldl (\n _fp -> [|| (1 +) ||] @@ n) (sint 0) sunit
        $   S.singleton (liftTyped ".")
        >>> S.files S.recursiveListDirectory
        >>> S.readByteString
        >>> S.unfold id (\bs k -> smaybe ([|| BS.uncons ||] @@ bs)
              (k Nothing)
              (\p -> spairElim p $ \x y -> k (Just (x, y))))
        :: GHCCode (R.ResourceT IO Int))

    print x
