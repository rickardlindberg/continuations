module Stages.Backend where

import qualified Stages.Backends.CArduino as CArduino
import qualified Stages.Backends.CPC as CPC

data Backend = CPC | CArduino

fromString :: String -> Maybe Backend
fromString "cpc"      = Just CPC
fromString "carduino" = Just CArduino
fromString _          = Nothing

toString :: Backend -> String
toString CPC      = "cpc"
toString CArduino = "carduino"

builtinsFor CPC      = CPC.exportedBuiltins
builtinsFor CArduino = CArduino.exportedBuiltins

generateAndCompileFor CPC      = CPC.generateAndCompile
generateAndCompileFor CArduino = CArduino.generateAndCompile
