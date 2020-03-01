module Hilcode.MonadFileSystem
( getFileSystemEntries
, currentDirectory
, mkDirectory
, Directory
, File
, AbsRel(..)
, MonadFileSystem
) where

import           Data.Vector (Vector)
import           TextShow (TextShow)

import qualified Data.Text
import qualified Data.Vector
import qualified System.Directory
import qualified TextShow

import           Hilcode.FileSystem

data FileSystemEntry (absRel :: AbsRel)
    = DirectoryEntry (Directory absRel)
    | FileEntry      (File absRel)

instance TextShow (FileSystemEntry absRel) where
    showb (FileEntry file)           = TextShow.showb file
    showb (DirectoryEntry directory) = TextShow.showb directory

instance MonadFileSystem IO DirEntry FileEntry where
    getFileSystemEntries :: DirEntry -> IO (Vector DirEntry, Vector FileEntry)
    getFileSystemEntries (AbsDir directory) = do
        (dirs, files) <- getFileSystemEntries directory
        pure (AbsDir <$> dirs, AbsFile <$> files)
    getFileSystemEntries (RelDir directory) =  do
        (dirs, files) <- getFileSystemEntries directory
        pure (RelDir <$> dirs, RelFile <$> files)

instance MonadFileSystem IO (Directory (absRel :: AbsRel)) (File (absRel :: AbsRel)) where
    getFileSystemEntries :: Directory absRel -> IO (Vector (Directory absRel), Vector (File absRel))
    getFileSystemEntries directory
        = toDirectoriesAndFiles <$> (System.Directory.listDirectory (toFilePath directory) >>= separateDirectoriesAndFiles directory)
          where
            separateDirectoriesAndFiles :: Directory absRel -> [FilePath] -> IO ([Directory absRel], [File absRel])
            separateDirectoriesAndFiles _ []                             = return ([], [])
            separateDirectoriesAndFiles directory (fileSystemEntry:rest) = do
                isDirectory <- System.Directory.doesDirectoryExist (toFilePath directory <> "/" <> fileSystemEntry)
                (directories, files) <- separateDirectoriesAndFiles directory rest
                if isDirectory
                    then pure (toDirectory directory fileSystemEntry : directories, files)
                    else pure (directories, toFile directory fileSystemEntry : files)
            toFilePath :: Directory absRel -> FilePath
            toFilePath directory = Data.Text.unpack (TextShow.showt directory)
            toFile :: Directory absRel -> FilePath -> File absRel
            toFile directory fileName = directory </> mkFile (Data.Text.pack fileName)
            toDirectory :: Directory absRel -> FilePath -> Directory absRel
            toDirectory directory directoryName = directory </> mkDirectory (Data.Text.pack directoryName)
            toDirectoriesAndFiles :: ([Directory absRel], [File absRel]) -> (Vector (Directory absRel), Vector (File absRel))
            toDirectoriesAndFiles (directories, files) = (Data.Vector.fromList directories, Data.Vector.fromList files)

class (Monad monad) => MonadFileSystem monad directory file where
    getFileSystemEntries :: directory -> monad (Vector directory, Vector file)
