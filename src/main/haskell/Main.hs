{-# LANGUAGE KindSignatures #-}
module Main where

import           Data.Vector (Vector)

import qualified Control.Monad
import qualified Data.Vector
import qualified TextShow

import           Hilcode.FileSystem
import           Hilcode.MonadFileSystem
import           Hilcode.Queue

data SpotConfiguration = SpotConfiguration {
    prunedDirectories :: Vector DirEntry
}

spotConfiguration :: SpotConfiguration
spotConfiguration = SpotConfiguration (Data.Vector.fromList (RelDir <$> [mkDirectory ".git", mkDirectory ".stack-work"]))

main :: IO ()
main = Control.Monad.void $ handleEvents spotConfiguration (singleton (InitialDirectoryFound (RelDir currentDirectory))) actionHandler

data FileSystemEvent
    = InitialDirectoryFound DirEntry
    | DirectoryFound DirEntry
    | FileFound FileEntry

data FileSystemAction
    = InitialDirectory DirEntry
    | OutputDirectory DirEntry
    | OutputFile FileEntry

instance EventHandler SpotConfiguration FileSystemEvent FileSystemAction where
    handleEvent _ (InitialDirectoryFound directory) = singleton (InitialDirectory directory)
    handleEvent _ (DirectoryFound directory)        = singleton (OutputDirectory directory)
    handleEvent _ (FileFound file)                  = singleton (OutputFile file)

actionHandler :: SpotConfiguration -> FileSystemAction -> IO (SpotConfiguration, Queue FileSystemEvent)
actionHandler state (InitialDirectory directory) =
    if directory `Data.Vector.elem` prunedDirectories state
    then pure (state, empty)
    else do
        (subdirectories, files) <- getFileSystemEntries directory
        pure (state, fromList (Data.Vector.toList ((FileFound <$> files) <> (DirectoryFound <$> subdirectories))))
actionHandler state (OutputDirectory directory) =
    if directory `Data.Vector.elem` prunedDirectories state
    then pure (state, empty)
    else do
        _ <- TextShow.printT directory
        (subdirectories, files) <- getFileSystemEntries directory
        pure (state, fromList (Data.Vector.toList ((FileFound <$> files) <> (DirectoryFound <$> subdirectories))))
actionHandler state (OutputFile file) = do
    _ <- TextShow.printT file
    pure (state, empty)

class EventHandler state event action where
    handleEvent :: state -> event -> Queue action

handleEvents :: forall monad state event action . Monad monad => EventHandler state event action
    => state
    -> Queue event
    -> (state -> action -> monad (state, Queue event))
    -> monad state
handleEvents state events actionHandler
    = step state events
  where
    step :: state -> Queue event -> monad state
    step state events = fst <$> go state events
      where
        go :: state -> Queue event -> monad (state, Queue event)
        go state events = case pop events of
            Nothing                  -> pure (state, mempty)
            Just (event, moreEvents) -> do
                (newState, newEvents) <- state `applyActions` handleEvent state event
                go newState (moreEvents <> newEvents)
    applyActions :: state -> Queue action -> monad (state, Queue event)
    applyActions = go mempty
      where
        go :: Queue event -> state -> Queue action -> monad (state, Queue event)
        go events state actions = case pop actions of
            Nothing                    -> pure (state, events)
            Just (action, moreActions) -> do
                (newState, newEvents) <- actionHandler state action
                go (events <> newEvents) newState moreActions
