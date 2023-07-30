module Loc
    ( ColumnRange
    , ModuleLoc (..)
    , FileLoc (..)
    , ModuleFileMap(..)
    , getPathOfModule
    , getModuleOfPath
    , toModuleLoc
    , toFileLoc
    ) where

import Data.Text (Text)
import Safe (headMay)

-- | Range, mapping start to end.
type ColumnRange = (Maybe Int, Maybe Int)

-- | Location in a module (may not have a corresponding source file).
data ModuleLoc = ModuleLoc
    { modName :: !(Maybe Text)
    , linenoM :: !(Maybe Int)
    , colrangeM :: !ColumnRange
    } deriving (Show, Eq, Ord)

-- | Location in a file (may not have a corresponding module).
data FileLoc = FileLoc
    { filepath :: !(Maybe FilePath)
    , linenoF :: !(Maybe Int)
    , colrangeF :: !ColumnRange
    } deriving (Show, Eq, Ord)

newtype ModuleFileMap = ModuleFileMap [(Text, FilePath)] deriving Show

instance Semigroup ModuleFileMap where
    ModuleFileMap a <> ModuleFileMap b = ModuleFileMap (a <> b)

instance Monoid ModuleFileMap where
    mempty = ModuleFileMap mempty

-- | Convert a module to a FilePath
getPathOfModule :: ModuleFileMap -> Text -> Maybe FilePath
getPathOfModule (ModuleFileMap ms) mod' = headMay [fp' | (mod'', fp') <- ms, mod'' == mod']

-- | Convert a FilePath to a Module
getModuleOfPath :: ModuleFileMap -> FilePath -> Maybe Text
getModuleOfPath (ModuleFileMap ms) fp = headMay [mod' | (mod', fp') <- ms, fp' == fp]

toModuleLoc :: ModuleFileMap -> FileLoc -> Maybe ModuleLoc
toModuleLoc mfm fl = go <$> fl.filepath
  where
    go = (\x -> ModuleLoc x fl.linenoF fl.colrangeF) . getModuleOfPath mfm

toFileLoc :: ModuleFileMap -> ModuleLoc -> Maybe FileLoc
toFileLoc mfm ml = go <$> ml.modName
  where
    go = (\x -> FileLoc x ml.linenoM ml.colrangeM) . getPathOfModule mfm
