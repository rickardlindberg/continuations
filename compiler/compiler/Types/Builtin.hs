module Types.Builtin where

import qualified Types.Types as T

data Builtin = Builtin
    { name  :: String
    , type_ :: T.Type
    }
