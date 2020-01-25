{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Staged.Stream.Directory (
    listDirectory,
    recursiveListDirectory,
    files,
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import System.FilePath ((</>))

import qualified System.Directory as Dir

import Staged.Commons
import Staged.Stream.Type
import Staged.Stream.Combinators (bfsTreeM, fromListM, filterM)

-- | List directory.
--
-- 'listDirectory' is defined using 'M.fromListM' of corresponding
-- function in the @directory@ package.
--
listDirectory :: MonadIO m => StreamM m FilePath FilePath
listDirectory = fromListM $ \(C dir) ->
    sliftIO $ C [|| fmap (map ($$dir </>)) (Dir.listDirectory $$dir) ||]

-- | Recursively (breath-first) walk through the directory structure.
recursiveListDirectory :: MonadIO m => StreamM m FilePath FilePath
recursiveListDirectory = bfsTreeM listDirectory $ \fp ->
    C [|| liftIO . Dir.doesDirectoryExist ||] @@ fp

-- | Filter with 'Dir.doesFileExist', i.e. return only existing files.
files :: MonadIO m => StreamM m a FilePath -> StreamM m a FilePath
files = filterM (\x -> sliftIO (C [|| Dir.doesFileExist ||] @@ x))
