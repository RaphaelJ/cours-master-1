{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>), (<|>), (<*))
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hPrint, stderr)
import Text.Parsec (
      ParseError, SourceName, between, eof, many, parse, string, try
    )
import Text.Parsec.Char (alphaNum, char, lower)
import Text.Parsec.Text (Parser)
import Text.Printf (printf)

data AST = Val Bool | Var Text | Not AST | And AST AST | Or AST AST | Empty

instance Show AST where
    show (Val True)        = "$true"
    show (Val False)       = "$false"
    show (Var name)        = T.unpack name
    show (Not expr)        = '~' : show expr
    show (And expr1 expr2) = printf "(%s & %s)" (show expr1) (show expr2)
    show (Or  expr1 expr2) = printf "(%s | %s)" (show expr1) (show expr2)
    show Empty             = "[]"

runParser :: SourceName -> Text ->  Either ParseError AST
runParser filename txt =
    parse formula filename noSpaces
  where
    noSpaces = T.filter (not . isSpace) txt -- Préprocesse les espaces.

    formula, expr, orExpr, andExpr, atom, variable :: Parser AST

    formula = (expr <* eof) <|> (eof >> return Empty)

    expr = do
        -- Fonctions de transformation des opérateurs secondaires en & et |.
        let p .=>.  q = Or (Not p) q
            p .<=.  q = Or p (Not q)
            p .<~>. q = Or (And p (Not q)) (And (Not p) q)
            p .<=>. q = Or (And p q) (And (Not p) (Not q))

        left <- orExpr

        (    (string "=>"        >> ((left .=>.)  <$> expr))
         <|> (try (string "<=")  >> ((left .<=.)  <$> expr))
         <|> (try (string "<~>") >> ((left .<~>.) <$> expr))
         <|> (string "<=>"       >> ((left .<=>.) <$> expr))
         <|> return left)

    orExpr = do
        left <- andExpr

        (    (char '|' >> (Or left <$> orExpr))
         <|> return left)

    andExpr = do
        left <- atom

        (    (char '&' >> (And left <$> andExpr))
         <|> return left)

    atom =     between (char '(') (char ')') expr
           <|> (char '~' >> (Not <$> atom))
           <|> (string "$true"  >> return (Val True))
           <|> (string "$false" >> return (Val False))
           <|> variable

    variable = do
        l  <- lower -- Lettre minuscule
        ls <- many (alphaNum <|> char '_')
        return (Var (T.pack (l : ls)))

main :: IO Int
main = do
    eAST <- runParser "stdin" <$> T.getContents
    case eAST of
        Left err  -> hPrint stderr err >> return 1
        Right ast -> print ast         >> return 0
