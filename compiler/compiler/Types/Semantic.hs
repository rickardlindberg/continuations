module Types.Semantic where

data Program = Program [Let]

data Let     = Let String Term

data Term    = Identifier String
             | Number     Integer
             | Lambda     [String] [Term]
             | BuiltinFn  Builtin

data Builtin = Builtin
    { name     :: String
    , code     :: String
    , fnType   :: Type
    , includes :: [String]
    }

data Type = TNumber | Fn [Type]
