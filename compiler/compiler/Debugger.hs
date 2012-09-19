import Graphics.UI.Gtk

import qualified Types.Semantic as Sem
import qualified Types.Syntax as Syn
import Stages.Analyze (syntaxToSemantic)
import Stages.CodeGen (generateCode)
import Stages.Parser (translate)
import Text.ParserCombinators.Parsec (parse)

main = do
    initGUI
    setupMainWindow
    mainGUI

setupMainWindow = do
    builder       <- builderFromFile "Debugger.glade"

    mainWindow    <- builderGetObject builder castToWindow   "windowMain"
    sourceText    <- builderGetObject builder castToTextView "textSource"
    syntaxText    <- builderGetObject builder castToTextView "textSyntax"
    semanticText  <- builderGetObject builder castToTextView "textSemantic"
    genText       <- builderGetObject builder castToTextView "textGen"

    sourceTextB   <- textViewGetBuffer sourceText
    syntaxTextB   <- textViewGetBuffer syntaxText
    semanticTextB <- textViewGetBuffer semanticText
    genTextB      <- textViewGetBuffer genText

    sourceTextB `onBufferChanged` do
        start <- textBufferGetStartIter sourceTextB
        end   <- textBufferGetEndIter sourceTextB
        input <- textBufferGetText sourceTextB start end True
        case parse translate "" input of
            Left  error   -> do
                textBufferSetText syntaxTextB   (show error)
                textBufferSetText semanticTextB ""
                textBufferSetText genTextB      ""
            Right program -> do
                let syntax   = syntaxToString program
                let semantic = semanticToString (syntaxToSemantic program)
                let gen      = generateCode (syntaxToSemantic program)
                textBufferSetText syntaxTextB   syntax
                textBufferSetText semanticTextB semantic
                textBufferSetText genTextB      gen


    mainWindow `onDestroy` mainQuit

    widgetShowAll mainWindow
    return ()

builderFromFile :: FilePath -> IO Builder
builderFromFile path = do
    builder <- builderNew
    builderAddFromFile builder path
    return builder

syntaxToString :: Syn.Program -> String
syntaxToString _ = "syn"

semanticToString :: Sem.Program -> String
semanticToString _ = "sem"
