module Hilcode.FileSystem
( Directory
, File
, AbsRel(..)
, DirEntry(..)
, FileEntry(..)
, rootDirectory
, currentDirectory
, mkDirectory
, mkFile
, (</>)
) where

import           Data.Text (Text)
import           TextShow (TextShow)

import qualified Data.Foldable
import qualified Data.Text
import qualified TextShow
import qualified TextShow.Data.Char

rootDirectory :: Directory 'Abs
rootDirectory = Root

currentDirectory :: Directory 'Rel
currentDirectory = Current

mkDirectory :: Text -> Directory 'Rel
mkDirectory directoryName
    | Data.Text.null directoryName = Current
    | directoryName == "."         = Current
    | otherwise                    = Data.Foldable.foldl' (</>) Current (toRelativeDirectory <$> Data.Text.splitOn "/" directoryName)
  where
    toRelativeDirectory :: Text -> Directory 'Rel
    toRelativeDirectory directoryName = Directory Current (DirectoryName directoryName)

mkFile :: Text -> File 'Rel
mkFile fileName = File Current (FileName fileName)

class Slash left right result | left right -> result where
    infixl 7 </>
    (</>) :: left -> right -> result

instance Slash (Directory (absRel :: AbsRel)) (File 'Rel) (File (absRel :: AbsRel)) where
    (</>) Root                                      (File parentDir fileName) = AbsoluteFile (Root </> parentDir) fileName
    (</>) absoluteDirectory@(AbsoluteDirectory _ _) (File parentDir fileName) = AbsoluteFile (absoluteDirectory </> parentDir) fileName
    (</>) Current                   file@(File _ _)           = file
    (</>) directory@(Directory _ _) (File parentDir fileName) = File (directory </> parentDir) fileName

instance Slash (Directory (absRel :: AbsRel)) (Directory 'Rel) (Directory (absRel :: AbsRel)) where
    (</>) Root                                      Current                                   = Root
    (</>) Root                                      (Directory parentDir directoryName)       = AbsoluteDirectory (Root </> parentDir) directoryName
    (</>) absoluteDirectory@(AbsoluteDirectory _ _) Current                                   = absoluteDirectory
    (</>) absoluteDirectory@(AbsoluteDirectory _ _) (Directory parentDir directoryName)       = AbsoluteDirectory (absoluteDirectory </> parentDir) directoryName
    (</>) Current                                   Current                                   = Current
    (</>) Current                                   directory@(Directory _ _)                 = directory
    (</>) directory@(Directory _ _)                 Current                                   = directory
    (</>) lftDirectory@(Directory _ _)              (Directory Current rgtDirectoryName)      = Directory lftDirectory rgtDirectoryName
    (</>) lftDirectory@(Directory _ _)              (Directory rgtParentDir rgtDirectoryName) = lftDirectory </> rgtParentDir </> Directory Current rgtDirectoryName

instance Slash (Directory (absRel :: AbsRel)) Text (Directory (absRel :: AbsRel)) where
    (</>) Root                                      directoryName = Root </> mkDirectory directoryName
    (</>) absoluteDirectory@(AbsoluteDirectory _ _) directoryName = absoluteDirectory </> mkDirectory directoryName
    (</>) Current                                   directoryName = mkDirectory directoryName
    (</>) directory@(Directory _ _)                 directoryName = directory </> mkDirectory directoryName

newtype FileName
    = FileName Text
    deriving (Eq, Ord)

newtype DirectoryName
    = DirectoryName Text
    deriving (Eq, Ord)

data AbsRel
    = Abs
    | Rel
    deriving (Eq, Ord)

data Directory (absRel :: AbsRel)
    = absRel ~ 'Abs => Root
    | absRel ~ 'Abs => AbsoluteDirectory (Directory 'Abs) DirectoryName
    | absRel ~ 'Rel => Current
    | absRel ~ 'Rel => Directory         (Directory 'Rel) DirectoryName

data File (absRel :: AbsRel)
    = absRel ~ 'Abs => AbsoluteFile (Directory 'Abs) FileName
    | absRel ~ 'Rel => File         (Directory 'Rel) FileName

instance Eq (Directory absRel) where
    Root                                         == Root                                         = True
    Root                                         == AbsoluteDirectory _         _                = False
    AbsoluteDirectory lftParent lftDirectoryName == AbsoluteDirectory rgtParent rgtDirectoryName = (lftParent == rgtParent) && (lftDirectoryName == rgtDirectoryName)
    AbsoluteDirectory _         _                == Root                                         = False
    Current                                      == Current                                      = True
    Current                                      == Directory         _         _                = False
    Directory         lftParent lftDirectoryName == Directory         rgtParent rgtDirectoryName = (lftParent == rgtParent) && (lftDirectoryName == rgtDirectoryName)
    Directory         _         _                == Current                                      = False

instance Eq (File absRel) where
    AbsoluteFile lftParent lftFileName == AbsoluteFile rgtParent rgtFileName = (lftParent == rgtParent) && (lftFileName == rgtFileName)
    File         lftParent lftFileName == File         rgtParent rgtFileName = (lftParent == rgtParent) && (lftFileName == rgtFileName)

instance TextShow DirectoryName where
    showb (DirectoryName directoryName) = TextShow.fromText directoryName

instance TextShow (Directory absRel) where
    showb Root                                      = TextShow.Data.Char.showbLitChar '/'
    showb (AbsoluteDirectory Root    directoryName) = TextShow.Data.Char.showbLitChar '/' <> TextShow.showb directoryName <> TextShow.Data.Char.showbLitChar '/'
    showb (AbsoluteDirectory parent  directoryName) = TextShow.showb parent <> TextShow.showb directoryName <> TextShow.Data.Char.showbLitChar '/'
    showb Current                                   = TextShow.Data.Char.showbLitChar '.' <> TextShow.Data.Char.showbLitChar '/'
    showb (Directory         Current directoryName) = TextShow.showb directoryName <> TextShow.Data.Char.showbLitChar '/'
    showb (Directory         parent  directoryName) = TextShow.showb parent <> TextShow.showb directoryName <> TextShow.Data.Char.showbLitChar '/'

instance TextShow FileName where
    showb (FileName fileName) = TextShow.fromText fileName

instance TextShow (File absRel) where
    showb (AbsoluteFile parent  fileName) = TextShow.showb parent <> TextShow.showb fileName
    showb (File         Current fileName) = TextShow.showb fileName
    showb (File         parent  fileName) = TextShow.showb parent <> TextShow.showb fileName

data DirEntry
    = AbsDir (Directory 'Abs)
    | RelDir (Directory 'Rel)

instance Eq DirEntry where
    (AbsDir lftDirectory) == (AbsDir rgtDirectory) = lftDirectory == rgtDirectory
    (RelDir lftDirectory) == (RelDir rgtDirectory) = lftDirectory == rgtDirectory
    (AbsDir _)            == (RelDir _)            = False
    (RelDir _)            == (AbsDir _)            = False

instance TextShow DirEntry where
    showb (AbsDir directory) = TextShow.showb directory
    showb (RelDir directory) = TextShow.showb directory

data FileEntry
    = AbsFile (File 'Abs)
    | RelFile (File 'Rel)

instance TextShow FileEntry where
    showb (AbsFile file) = TextShow.showb file
    showb (RelFile file) = TextShow.showb file
