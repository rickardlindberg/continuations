module Types.Semantic where

import qualified Types.Types as T

data Program = Program [Let]

data Let     = Let String Term

data Term    = Identifier String
             | Number     Integer
             | Lambda     [String] [Term]
             | BuiltinFn  Builtin

data Builtin = Builtin
    { name     :: String
    , code     :: String
    , fnType   :: T.Type
    , includes :: [String]
    }
