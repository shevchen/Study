module Main where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import System.Environment
import System.IO
import Lambda hiding (main)

def = emptyDef{ identStart      = letter
              , identLetter     = alphaNum
              , opStart         = oneOf ".\\-="
              , opLetter        = oneOf ".\\->="
              , reservedOpNames = ["\\", ".", "->", "="]
              , reservedNames   = ["let", "in"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reserved   = m_reserved
           , reservedOp = m_reservedOp
           , whiteSpace = m_whiteSpace } = makeTokenParser def

onlyApply :: Parser Term
onlyApply = chainl1 termparser (do
    { m_whiteSpace
    ; return App
    })

termparser :: Parser Term
termparser = do
    { term <- m_parens termparser
    ; return term 
    }
    <|> do
    { m_reserved "let"
    ; var <- m_identifier
    ; m_reservedOp "="
    ; expr <- onlyApply
    ; m_reserved "in"
    ; right <- onlyApply
    ; return (App (Abs var right) expr)
    }
    <|> do
    { m_reservedOp "\\"
    ; var <- m_identifier
    ; (m_reservedOp ".") <|> (m_reservedOp "->")
    ; term <- onlyApply
    ; return (Abs var term)
    }
    -- <|> do
    --{ onlyApply
    --}
    <|> do
    { a <- m_identifier
    ; return (Var a)
    }

main :: IO ()
main = do
    args <- getArgs
    case parse termparser "" (head args) of
        Left err -> print err
        Right ans -> print ans
