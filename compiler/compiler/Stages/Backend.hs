module Stages.Backend where

import qualified Stages.Backends.CArduino as CArduino
import qualified Stages.Backends.CCommon as CCommon
import qualified Stages.Backends.CPC as CPC

data Backend = CPC | CArduino

fromString :: String -> Maybe Backend
fromString "cpc"      = Just CPC
fromString "carduino" = Just CArduino
fromString _          = Nothing

toString :: Backend -> String
toString CPC      = "cpc"
toString CArduino = "carduino"

generateAndCompile CPC      = CPC.generateAndCompile
generateAndCompile CArduino = CArduino.generateAndCompile

builtins CPC      = CCommon.getBuiltins
builtins CArduino = CCommon.getBuiltins
