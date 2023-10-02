module Loc
    ( ColumnRange
    , ModuleLoc (..)
    , FileLoc (..)
    , ModuleFileMap
    , moduleFileMapFromList
    , moduleFileMapAssocs
    , getPathOfModule
    , getModuleOfPath
    , toModuleLoc
    , toFileLoc
    , HasSourceRange (..)
    , SourceRange (..)
    , unknownSourceRange
    , isLineInside
    , srFromLineNo
    , singleify
    ) where

import Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Safe (headMay)

-- ------------------------------------------------------------------------------------------------

-- | Range, mapping start to end.
type ColumnRange = (Maybe Int, Maybe Int)

data SourceRange = SourceRange
    { startLine :: !(Maybe Int)
    , startCol :: !(Maybe Int)
    , endLine :: !(Maybe Int)
    , endCol :: !(Maybe Int)
    }
    deriving (Show, Eq, Ord)

-- | A source range that represents an unknown location.
unknownSourceRange :: SourceRange
unknownSourceRange = SourceRange Nothing Nothing Nothing Nothing

srFromLineNo :: Int -> SourceRange
srFromLineNo lineno = unknownSourceRange{startLine = Just lineno, endLine = Just lineno}

-- | Return whether a given line number lies within a given source range.
isLineInside :: SourceRange -> Int -> Bool
isLineInside SourceRange{startLine = Just sl, endLine = Just el} num = num >= sl && num <= el
isLineInside SourceRange{startLine = Just sl, endLine = Nothing} num = num >= sl
isLineInside _ _ = False

-- | Convert a 'SourceRange' to potentially a single line and 'ColumnRange'.
singleify :: SourceRange -> Maybe (Int, ColumnRange)
singleify sr
    | isNothing sl = Nothing
    | sl == endLine sr = do
        lineno <- sl
        pure (lineno, (startCol sr, endCol sr))
    | otherwise = Nothing
  where
    sl = startLine sr

-- ------------------------------------------------------------------------------------------------

-- | Location in a module (may not have a corresponding source file).
data ModuleLoc = ModuleLoc
    { modName :: !T.Text
    , mSourceRange :: !SourceRange
    }
    deriving (Show, Eq, Ord)

-- | Location in a file (may not have a corresponding module).
data FileLoc = FileLoc
    { filepath :: !FilePath
    , fSourceRange :: !SourceRange
    }
    deriving (Show, Eq, Ord)

class HasSourceRange a where
    -- | Retrieve the source range from this source location.
    sourceRange :: a -> SourceRange

instance HasSourceRange FileLoc where
    sourceRange = fSourceRange

instance HasSourceRange ModuleLoc where
    sourceRange = mSourceRange

newtype ModuleFileMap = ModuleFileMap (Map.Map T.Text FilePath) deriving (Show, Eq)

instance Semigroup ModuleFileMap where
    ModuleFileMap a <> ModuleFileMap b = ModuleFileMap $ a <> b

instance Monoid ModuleFileMap where
    mempty = ModuleFileMap mempty

moduleFileMapFromList :: [(T.Text, FilePath)] -> ModuleFileMap
moduleFileMapFromList = ModuleFileMap . Map.fromList

-- | Return mappings between a module name and a filepath.
moduleFileMapAssocs :: ModuleFileMap -> [(T.Text, FilePath)]
moduleFileMapAssocs (ModuleFileMap map_) = Map.assocs map_

-- | Convert a module to a FilePath.
getPathOfModule :: ModuleFileMap -> T.Text -> Maybe FilePath
getPathOfModule (ModuleFileMap ms) mod' = Map.lookup mod' ms

-- | Convert a FilePath to a Module.
getModuleOfPath :: ModuleFileMap -> FilePath -> Maybe T.Text
getModuleOfPath (ModuleFileMap ms) fp = headMay [mod' | (mod', fp') <- Map.assocs ms, fp' == fp]

-- | Convert a 'FileLoc' to a 'ModuleLoc'.
toModuleLoc :: ModuleFileMap -> FileLoc -> Maybe ModuleLoc
toModuleLoc mfm fl = convert fl.filepath
  where
    makeModuleLoc txt' = ModuleLoc txt' (sourceRange fl)
    convert fp = makeModuleLoc <$> getModuleOfPath mfm fp

-- | Convert a 'ModuleLoc' to a 'FileLoc'.
toFileLoc :: ModuleFileMap -> ModuleLoc -> Maybe FileLoc
toFileLoc mfm ml = convert ml.modName
  where
    makeFileLoc txt' = FileLoc txt' (sourceRange ml)
    convert mn = makeFileLoc <$> getPathOfModule mfm mn
