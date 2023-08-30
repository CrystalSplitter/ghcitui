module Loc
    ( ColumnRange
    , ModuleLoc (..)
    , FileLoc (..)
    , ModuleFileMap (..)
    , getPathOfModule
    , getModuleOfPath
    , toModuleLoc
    , toFileLoc
    ) where

import qualified Data.Text as T
import Safe (headMay)

-- | Range, mapping start to end.
type ColumnRange = (Maybe Int, Maybe Int)

-- | Location in a module (may not have a corresponding source file).
data ModuleLoc = ModuleLoc
    { modName :: !(Maybe T.Text)
    , linenoM :: !(Maybe Int)
    , colrangeM :: !ColumnRange
    }
    deriving (Show, Eq, Ord)

-- | Location in a file (may not have a corresponding module).
data FileLoc = FileLoc
    { filepath :: !(Maybe FilePath)
    , linenoF :: !(Maybe Int)
    , colrangeF :: !ColumnRange
    }
    deriving (Show, Eq, Ord)

newtype ModuleFileMap = ModuleFileMap [(T.Text, FilePath)] deriving (Show)

instance Semigroup ModuleFileMap where
    ModuleFileMap a <> ModuleFileMap b = ModuleFileMap $ a <> b

instance Monoid ModuleFileMap where
    mempty = ModuleFileMap mempty

-- | Convert a module to a FilePath.
getPathOfModule :: ModuleFileMap -> T.Text -> Maybe FilePath
getPathOfModule (ModuleFileMap ms) mod' = headMay [fp' | (mod'', fp') <- ms, mod'' == mod']

-- | Convert a FilePath to a Module.
getModuleOfPath :: ModuleFileMap -> FilePath -> Maybe T.Text
getModuleOfPath (ModuleFileMap ms) fp = headMay [mod' | (mod', fp') <- ms, fp' == fp]

-- | Convert a 'FileLoc' to a 'ModuleLoc'.
toModuleLoc :: ModuleFileMap -> FileLoc -> Maybe ModuleLoc
toModuleLoc mfm fl = convert <$> fl.filepath
  where
    makeModuleLoc txt' = ModuleLoc txt' fl.linenoF fl.colrangeF
    convert = makeModuleLoc . getModuleOfPath mfm

-- | Convert a 'ModuleLoc' to a 'FileLoc'.
toFileLoc :: ModuleFileMap -> ModuleLoc -> Maybe FileLoc
toFileLoc mfm ml = convert <$> ml.modName
  where
    makeFileLoc txt' = FileLoc txt' ml.linenoM ml.colrangeM
    convert = makeFileLoc . getPathOfModule mfm
