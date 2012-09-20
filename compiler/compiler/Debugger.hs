import Graphics.UI.Gtk

import Control.Monad
import Control.Monad.Trans.State.Lazy as ST
import qualified Types.Semantic as Sem
import qualified Types.Syntax as Syn
import Stages.Analyze (syntaxToSemantic)
import Stages.Backends.CCommon (generateCode)
import Stages.Parser (translate)
import Text.ParserCombinators.Parsec (parse)

main = do
    initGUI
    setupMainWindow
    mainGUI

setupMainWindow = do
    builder       <- builderFromFile "debugger-interface.glade"

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
                let syntax      = syntaxToString program
                let (Right sem) = syntaxToSemantic program
                let semantic    = semanticToString sem
                let gen         = generateCode False sem
                textBufferSetText syntaxTextB   syntax
                textBufferSetText semanticTextB semantic
                textBufferSetText genTextB      gen

    text <- readFile "examples/music.con"
    textBufferSetText sourceTextB text

    mainWindow `onDestroy` mainQuit

    widgetShowAll mainWindow
    return ()

builderFromFile :: FilePath -> IO Builder
builderFromFile path = do
    builder <- builderNew
    builderAddFromFile builder path
    return builder

syntaxToString :: Syn.Program -> String
syntaxToString = genDoc . writeProgram
    where
        writeProgram (Syn.Program lets) = mapM_ writeLet lets
        writeLet (Syn.Let name term) = do
            writeLine "LET"
            indent
            writeLine $ "name = " ++ name
            write "term = "
            indent
            writeTerm term
            dedent
            dedent
            writeLine ""
        writeTerm (Syn.Identifier name) = writeLine $ "IDENT " ++ name
        writeTerm (Syn.Number number) = writeLine $ "NUMBER " ++ show number
        writeTerm (Syn.Lambda lets args terms) = do
            writeLine "LAMBDA"
            indent
            writeLine "lets = "
            indent
            mapM_ writeLet lets
            dedent
            writeLine "args = "
            indent
            mapM_ writeLine args
            dedent
            writeLine "terms = "
            indent
            mapM_ writeTerm terms
            dedent
            dedent

semanticToString :: Sem.Program -> String
semanticToString = genDoc . writeProgram
    where
        writeProgram (Sem.Program lets) = mapM_ writeLet lets
        writeLet (Sem.Let name term) = do
            writeLine "LET"
            indent
            writeLine $ "name = " ++ name
            write "term = "
            indent
            writeTerm term
            dedent
            dedent
            writeLine ""
        writeTerm (Sem.Identifier name) = writeLine $ "IDENT " ++ name
        writeTerm (Sem.Number number) = writeLine $ "NUMBER " ++ show number
        writeTerm (Sem.Function type_ body) = do
            writeLine "FUNCTION"
            indent
            writeLine "type = ..."
            writeLine "body ="
            indent
            writeBody body
            dedent
            dedent
        writeBody (Sem.Lambda args terms) = do
            writeLine "LAMBDA"
            indent
            writeLine "args = "
            indent
            mapM_ writeLine args
            dedent
            writeLine "terms = "
            indent
            mapM_ writeTerm terms
            dedent
            dedent
        writeBody (Sem.Builtin includes code) = do
            writeLine "BUILTIN"
            indent
            writeLine "includes = ..."
            writeLine "code = ..."
            dedent

-- Document writing langauge

data Document = Document
    { numIndent :: Int
    , content   :: String
    }

genDoc :: ST.State Document a -> String
genDoc m = content $ ST.execState m (Document 0 "")

writeLine :: String -> ST.State Document ()
writeLine line = write line >> write "\n"

write :: String -> ST.State Document ()
write text = do
    doc <- ST.get
    when (null (content doc) || (last (content doc)) == '\n') $ do
        write_ $ concatMap (const "    ") [1..numIndent doc]
    write_ text

write_ :: String -> ST.State Document ()
write_ text = ST.modify (\s -> s { content = content s ++ text })

indent :: ST.State Document ()
indent = ST.modify (\s -> s { numIndent = numIndent s + 1 })

dedent :: ST.State Document ()
dedent = ST.modify (\s -> s { numIndent = numIndent s - 1 })
