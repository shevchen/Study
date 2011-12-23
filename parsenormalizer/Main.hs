import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import System.Environment
import System.IO
import Lambda hiding (main)

def = emptyDef{ identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf ".\\-"
              , opLetter = oneOf ".\\->"
              , reservedOpNames = ["\\", ".", "->"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , whiteSpace = m_whiteSpace } = makeTokenParser def

parser :: Parser Term
parser = m_whiteSpace >> termparser <* eof where
    termparser :: Parser Term
    termparser = do
        { inside <- m_parens termparser
        ; option inside (do {other <- termparser; return (App inside other)})
        }
        <|> do
        { m_reservedOp "\\"
        ; var <- m_identifier
        ; (m_reservedOp ".") <|> (m_reservedOp "->")
        ; tm <- termparser
        ; return (Abs var tm)
        }
        {-<|> do
        { a <- termparser
        ; b <- termparser
        ; return (App a b)
        }-}
        <|> do
        { a <- m_identifier
        ; return (Var a)
        }

main :: IO ()
main = do
    args <- getArgs
    case parse parser "" (head args) of
        Left err -> print err
        Right ans -> print ans
