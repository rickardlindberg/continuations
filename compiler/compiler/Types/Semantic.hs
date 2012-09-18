module Types.Semantic where

import qualified Types.Types as T

data Program = Program [Let]

data Let     = Let String Term

data Term    = Identifier String
             | Number     Integer
             | Function   Fn

data Fn      = Fn
    { fnTypeNew :: T.Type
    , fnBodyNew :: FnBody
    }

data FnBody =
    Lambda
        { args     :: [String]
        , terms    :: [Term]
        }
    | Builtin
        { name     :: String
        , code     :: String
        , fnType   :: T.Type
        , includes :: [String]
        }
