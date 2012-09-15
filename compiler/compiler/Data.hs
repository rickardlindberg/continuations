module Data where

data Program  = Program [Function]

data Function = Function String Lambda

data Lambda   = Lambda [String] [Term]

data Term     = Identifier String
              | Number     Integer
              | TermLambda Lambda
