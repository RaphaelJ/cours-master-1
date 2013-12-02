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

data Litteral = Lit Text | OppLit Text deriving (Eq, Ord)

type Clause = S.Set Litteral

data Solution = Consistant | Inconsistant

instance Show AST where
    show (Val True)        = "$true"
    show (Val False)       = "$false"
    show (Var name)        = T.unpack name
    show (Not expr)        = '~' : show expr
    show (And expr1 expr2) = printf "(%s & %s)" (show expr1) (show expr2)
    show (Or  expr1 expr2) = printf "(%s | %s)" (show expr1) (show expr2)
    show Empty             = "[]"

instance Show Litteral where
    show (Lit name)    = T.unpack name
    show (OppLit name) = '~' : T.unpack name

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
    removeInclusive . removeValid . clauses . distribute . simplifyNot . deMorgan
  where
    -- Applique les règles de DeMorgan pour propager les négations vers les
    -- feuilles de l'arbre.
    deMorgan (Not p) | And r s <- p' = Or  (deMorgan (Not r)) (deMorgan (Not s))
                     | Or  r s <- p' = And (deMorgan (Not r)) (deMorgan (Not s))
                     | otherwise     = Not p'
      where
        p' = deMorgan p
    deMorgan (And p q) = And (deMorgan p) (deMorgan q)
    deMorgan (Or  p q) = Or  (deMorgan p) (deMorgan q)
    deMorgan p         = p

    -- Supprime les doubles négations de l'arbre syntaxique.
    simplifyNot (Not (Not p)) = simplifyNot p
    simplifyNot (And p q)     = And (simplifyNot p) (simplifyNot q)
    simplifyNot (Or  p q)     = Or  (simplifyNot p) (simplifyNot q)
    simplifyNot p             = p

    -- Applique les règles de simple distributivité en commençant en profondeur
    -- pour faire remonter les conjonctions.
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

    -- Extrait les clauses d'un arbre syntaxique en CNF à l'aide d'un parcours
    -- en profondeur.
    clauses :: AST -> [Clause]
    clauses =
        goConj []
      where
        -- Ajoute un nouvel ensemble de littéraux pour chaque terme de chaque
        -- conjonction.
        goConj acc (And p q) = goConj (goConj acc q) p
        goConj acc p         = goDisj (S.empty) p : acc

        -- Rajoute tous les littéraux des sous-disjonctions à l'ensemble des
        -- littéraux de la clause en cours de parcours.
        goDisj clause (Or p q)          = let clause' = goDisj clause p
                                          in goDisj clause' q
        goDisj clause (Var name)        = S.insert (Lit name) clause
        goDisj clause ~(Not (Var name)) = S.insert (OppLit name) clause

    -- Supprime les clauses contenant au moins une paire complémentaire de
    -- littéraux.
    -- Complexité : O(n * m * log m) où n est le nombre de clauses et m le
    -- nombre de littéraux par clause.
    removeValid :: [Clause] -> [Clause]
    removeValid =
        filter (not . isValid)
      where
        -- Vaut True si la clause c contient une paire complémentaire.
        -- Complexité : O(n * log n) où n est le nombre de littéraux de c.
        isValid c = any ((`S.member` c) . complement) (S.toList c)

    -- Supprime les clauses dont il existe une seconde clause composée d'un
    -- sous-ensemble de leurs littéraux.
    -- Complexité : O(n² * m) où n est le nombre de clauses et m le nombre de
    -- littéraux par clause.
    removeInclusive :: [Clause] -> [Clause]
    removeInclusive =
        go []
      where
        go acc []       = acc
        go acc (c : cs) |    any (`includedIn` c) acc
                          || any (`includedIn` c) cs = go acc cs
                        | otherwise                  = go (c : acc) cs

        -- Retourne True si la clause c2 comprends tous les littéraux de c1.
        -- Complexité : O(n) où n est le nombre de littéraux par clause.
        -- L'algorithme de différence s'exécute en temps linéaire en parcourant
        -- simultanément les deux arbres binaires.
        c1 `includedIn` c2 = S.null (S.difference c1 c2)
        -- Complexité : O(n * log n) où n est le nombre de littéraux par clause.
        -- c1 `includedIn` c2 = all (`S.member` c2) (S.toList c1)

complement :: Litteral -> Litteral
complement (Lit name)    = OppLit name
complement (OppLit name) = Lit name

resolve :: [Clause] -> ([Clause], Solution)
resolve clauses =
    let derivs = [ S.delete lit1 c1 `S.union` S.delete lit2 c2
                  | c1 <- clauses, lit1 <- S.fromList c1, c2 <- clauses
                  , let lit2 = complement lit1, lit2 `S.member` c2 ]
        clauses' = derivs ++ clauses
    in case derivs of
            []                 -> (clauses', Inconsistant)
            ds | any S.null ds -> (clauses', Consistant)
               | otherwise     -> resolve clauses'

main :: IO Int
main = do
    eAST <- runParser "stdin" <$> T.getContents
    case eAST of
        Left err  -> hPrint stderr err >> return 1
        Right ast -> print (normalize ast) >> return 0
