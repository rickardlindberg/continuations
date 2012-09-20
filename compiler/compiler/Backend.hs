module Backend where

import qualified Backends.CArduino as CArduino
import qualified Backends.CPC as CPC

data Backend = CPC | CArduino

fromString :: String -> Maybe Backend
fromString "cpc"      = Just CPC
fromString "carduino" = Just CArduino
fromString _          = Nothing

toString :: Backend -> String
toString CPC      = "cpc"
toString CArduino = "carduino"

--generateAndCompile :: Backend -> IO ()
generateAndCompile CPC      = CPC.generateAndCompile
generateAndCompile CArduino = CArduino.generateAndCompile
