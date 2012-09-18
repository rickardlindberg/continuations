module Types.Types where

data Type = Number
          | Function [Type]
