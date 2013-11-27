{-# LANGUAGE OverloadedStrings, PatternGuards #-}

import Control.Applicative ((<$>), (<|>), (<*))
import Data.Char (isSpace)
import qualified Data.Set as S
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

data Litteral = Lit Text | Opp Text

type Clause = S.Set Litteral

instance Show AST where
    show (Val True)        = "$true"
    show (Val False)       = "$false"
    show (Var name)        = T.unpack name
    show (Not expr)        = '~' : show expr
    show (And expr1 expr2) = printf "(%s & %s)" (show expr1) (show expr2)
    show (Or  expr1 expr2) = printf "(%s | %s)" (show expr1) (show expr2)
    show Empty             = "[]"

runParser :: SourceName -> Text -> Either ParseError AST
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

simplify :: AST -> AST
simplify p@(Val _) = p
simplify p@(Var _) = p
simplify (Not p) | Val True  <- p' = Val False
                 | Val False <- p' = Val True
                 | Not q     <- p' = q
                 | otherwise       = Not p
  where
    p' = simplify p
simplify (And p q) | Val True  <- p' = q'
                   | Val False <- p' = Val False
                   | Val True  <- q' = p'
                   | Val False <- q' = Val False
                   | otherwise       = And p' q'
  where
    p' = simplify p
    q' = simplify q
simplify (Or p q) | Val True  <- p' = Val True
                  | Val False <- p' = q'
                  | Val True  <- q' = Val True
                  | Val False <- q' = p'
                  | otherwise       = Or p' q'
  where
    p' = simplify p
    q' = simplify q
simplify Empty = Empty

-- normalize :: AST -> [Clause]
-- normalize (Val True)  = []
-- -- normalize (Val False) = [S.fromList (Val False)]
-- normalize Empty       = []
normalize =
    distribute . simplifyNot . deMorgan
  where
    deMorgan (Not p) | And r s <- p' = Or  (deMorgan (Not r)) (deMorgan (Not s))
                     | Or  r s <- p' = And (deMorgan (Not r)) (deMorgan (Not s))
                     | otherwise     = Not p'
      where
        p' = deMorgan p
    deMorgan (And p q) = And (deMorgan p) (deMorgan q)
    deMorgan (Or  p q) = Or  (deMorgan p) (deMorgan q)
    deMorgan p         = p

    simplifyNot (Not (Not p)) = simplifyNot p
    simplifyNot (And p q)     = And (simplifyNot p) (simplifyNot q)
    simplifyNot (Or  p q)     = Or  (simplifyNot p) (simplifyNot q)
    simplifyNot p             = p

    -- Applique une distributivité en commençant en profondeur pour faire
    -- remonter les conjonctions. Applique les règles de simple et de double
    -- distributivité.
    distribute (Or p q)
--         | And r s <- p'
--         , And t u <- q' = And (And (distribute (Or r t)) (distribute (Or r u)))
--                               (And (distribute (Or s t)) (distribute (Or s u)))
        | And r s <- q' = And (distribute (Or p' r)) (distribute (Or p' s))
        | And r s <- p' = And (distribute (Or r q')) (distribute (Or s q'))
        | otherwise     = Or p' q'
      where
        p' = distribute p
        q' = distribute q
    distribute (And p q) = And (distribute p) (distribute q)
    distribute (Not p)   = Not (distribute p)
    distribute p         = p

main :: IO Int
main = do
    eAST <- runParser "stdin" <$> T.getContents
    case eAST of
        Left err  -> hPrint stderr err >> return 1
        Right ast -> print (normalize ast) >> return 0
