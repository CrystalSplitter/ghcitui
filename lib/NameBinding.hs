module NameBinding where

import Data.Text (Text, concat)
import Prelude hiding (concat)

data BindingValue a = Uneval | Evald a deriving (Eq, Show)

-- | Represents a binding in the local context.
data NameBinding t = NameBinding
    { bName :: t
    -- ^ Name of the binding.
    , bType :: t
    -- ^ Type of the binding.
    , bValue :: BindingValue t
    -- ^ Value of the binding.
    }
    deriving (Eq, Show)

-- | Display the name bindings together into a group of Texts.
renderNamesTxt :: (Functor f, Foldable f) => f (NameBinding Text) -> f Text
renderNamesTxt ns = onEach <$> ns
  where
    valueRender Uneval = "_"
    valueRender (Evald v) = v
    onEach nb = concat [bName nb, " :: ", bType nb, " = ", valueRender . bValue $ nb]
