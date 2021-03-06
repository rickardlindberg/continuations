module Types.Syntax where

data Program = Program [Let]

data Let     = Let String Term

data Term    = Identifier String
             | Number     Integer
             | Lambda     [Let] [String] [Term]
