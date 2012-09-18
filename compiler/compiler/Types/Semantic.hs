module Types.Semantic where

import qualified Types.Types as T

data Program = Program [Let]

data Let     = Let String Term

data Term    = Identifier String
             | Number     Integer
             | Function   T.Type FnBody

data FnBody  = Lambda  { args     :: [String]
                       , terms    :: [Term]
                       }
             | Builtin { includes :: [String]
                       , code     :: String
                       }
