import qualified Lexer
import qualified AST
import qualified Types

main = do
    Lexer.test
    AST.test
    Types.test
