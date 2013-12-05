{-# LANGUAGE OverloadedStrings, PatternGuards #-}

import Control.Applicative ((<$>), (<|>), (<*))
import Control.Monad (forever, forM_)
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hPrint, stderr)
import Text.Parsec (
      ParseError, SourceName, between, eof, many, optionMaybe, parse, string
    , try
    )
import Text.Parsec.Char (alphaNum, char, lower)
import Text.Parsec.Text (Parser)
import Text.Printf (printf)

-- Types -----------------------------------------------------------------------

-- Contient l'arbre syntaxique d'une formule non vide.
-- Les opérateurs =>, <=,<=> et <~> sont convertis en disjonctions et
-- conjonctions au parsing et n'existent pas dans l'arbre syntaxique.
data Formula = Val Bool
             | Var Text
             | Not Formula
             | And Formula Formula
             | Or  Formula Formula

-- Permet d'afficher une formule.
-- Évite les parenthèses inutiles. Par exemple, (a | b) | c devient a | b | c.
instance Show Formula where
    show (Val True)  = "$true"
    show (Val False) = "$false"

    show (Var name)  = T.unpack name

    show (Not expr@(And _ _)) = printf "~(%s)" (show expr)
    show (Not expr@(Or  _ _)) = printf "~(%s)" (show expr)
    show (Not expr)           = '~' : show expr

    show (And expr1 expr2)    =
        printf "%s & %s" (inner expr1) (inner expr2)
      where
        inner expr@(Or  _ _) = printf "(%s)" (show expr)
        inner expr           = show expr

    show (Or  expr1 expr2)    =
        printf "%s | %s" (inner expr1) (inner expr2)
      where
        inner expr@(And _ _) = printf "(%s)" (show expr)
        inner expr           = show expr

-- Représentation des littéraux dans les clauses des CNFs.
data Litteral = Lit Text | OppLit Text
    deriving (Eq, Ord)

instance Show Litteral where
    show (Lit name)    =       T.unpack name
    show (OppLit name) = '~' : T.unpack name

-- Retourne le complément d'un littéral.
complement :: Litteral -> Litteral
complement (Lit name)    = OppLit name
complement (OppLit name) = Lit name

-- Chaque clause est un ensemble (Set) de littéraux. Set est une collection
-- d'éléments uniques stockées dans un arbre binaire.
-- Cette structure de données permet de rapidement rechercher l'existence d'un
-- littéral dans une clause, d'effectuer les opérations d'union en temps
-- linéaire (par rapport au nombre de littéraux) et les opérations de
-- suppression en temps logarithmique (utiles pour l'application de la méthode
-- de résolution).
type Clause = S.Set Litteral

emptyClause :: Clause
emptyClause = S.empty

-- Contient les deux clauses dérivées à partir desquelles une dérivée a été
-- créée s'il ne s'agit pas d'une clause de la CNF, de telle manière à former
-- un arbre.
data Derivative = InitialClause Clause
                | Derivative    Clause (Derivative, Derivative)
    deriving (Show, Eq, Ord)

-- Parseur ---------------------------------------------------------------------

-- Tente de parser le fichier nommé 'filename' (utilisé dans les messages
-- d'erreur) ayant 'txt' comme contenu.
-- Retourne soit une formule, soit une erreur de parsing.
-- Pour une formule vide, retourne Nothing.
runParser :: SourceName -> Text -> Either ParseError (Maybe Formula)
runParser filename txt =
    parse formula filename noSpaces
  where
    noSpaces = T.filter (not . isSpace) txt -- Préprocesse les espaces.

    formula :: Parser (Maybe Formula) -- Retourne une formule ou Nothing pour
    formula = optionMaybe expr <* eof -- une formule vide.

    expr, orExpr, andExpr, atom, variable :: Parser Formula
    expr = do
        -- Fonctions de transformation des opérateurs secondaires en & et |.
        let p .=>.  q = Or (Not p) q
            p .<=.  q = Or p (Not q)
            p .<~>. q = Or (And p (Not q)) (And (Not p) q)
            p .<=>. q = Or (And p q) (And (Not p) (Not q))

        left <- orExpr

        (    (try (string "<~>") >> ((left .<~>.) <$> expr))
         <|> (try (string "<=>") >> ((left .<=>.) <$> expr))
         <|> (string "<="        >> ((left .<=.)  <$> expr))
         <|> (string "=>"        >> ((left .=>.)  <$> expr))
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
           <|> (try (string "$true") >> return (Val True))
           <|> (     string "$false" >> return (Val False))
           <|> variable

    variable = do
        l  <- lower -- Lettre minuscule
        ls <- many (alphaNum <|> char '_')
        return (Var (T.pack (l : ls)))

-- Résolution ------------------------------------------------------------------

-- Simplifie une formule en faisant remonter ses valeurs 'true'/'false' à la
-- racine de l'arbre.
-- Il est aisé de prouver par induction que cet algorithme génère un arbre où
-- seule la racine peut contenir une valeur 'true' ou 'false', celles se
-- trouvant plus bas dans l'arbre étant systématiquement et récursivement soit
-- simplifiées, soit remontées au niveau supérieur.
simplify :: Formula -> Formula
simplify (Not p) | Val True  <- p' = Val False
                 | Val False <- p' = Val True
                 | Not q     <- p' = q
                 | otherwise       = Not p'
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
simplify p = p

-- Retourne la formule en CNF.
-- La formule en entrée doit avoir été simplifiée (elle ne doit plus contenir de
-- valeur true/false en dehors de sa racine).
normalize :: Formula -> [Clause]
normalize (Val True)  = []
normalize (Val False) = [emptyClause]
normalize formula     =
      removeInclusive
    $ removeValid
    $ clauses
    $ distribute
    $ doubleNot
    $ deMorgan formula
  where
    -- Applique les règles de DeMorgan pour propager les négations vers les
    -- feuilles de l'arbre syntaxique.
    deMorgan (Not p) | And r s <- p' = Or  (deMorgan (Not r)) (deMorgan (Not s))
                     | Or  r s <- p' = And (deMorgan (Not r)) (deMorgan (Not s))
                     | otherwise     = Not p'
      where
        p' = deMorgan p
    deMorgan (And p q) = And (deMorgan p) (deMorgan q)
    deMorgan (Or  p q) = Or  (deMorgan p) (deMorgan q)
    deMorgan p         = p

    -- Supprime les doubles négations de l'arbre syntaxique.
    doubleNot (Not (Not p)) = doubleNot p
    doubleNot (And p q)     = And (doubleNot p) (doubleNot q)
    doubleNot (Or  p q)     = Or  (doubleNot p) (doubleNot q)
    doubleNot p             = p

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
    clauses :: Formula -> [Clause]
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
    removeValid = filter (not . isValid)

    -- Supprime les clauses dont il existe une seconde clause composée d'un
    -- sous-ensemble de leurs littéraux. Supprime également les exemplaires
    -- multiples d'une même clause.
    -- Complexité : O(n² * m) où n est le nombre de clauses et m le nombre de
    -- littéraux par clause. L'algorithme de détection des sous-ensembles
    -- s'exécute en temps linéaire en parcourant simultanément les deux arbres
    -- binaires (algorithme similaire à la fusion du merge-sort).
    removeInclusive :: [Clause] -> [Clause]
    removeInclusive =
        go []
      where
        go acc []       = acc
        go acc (c : cs) |    any (`S.isSubsetOf` c) acc
                          || any (`S.isSubsetOf` c) cs  = go acc cs
                        | otherwise                     = go (c : acc) cs

-- Applique la méthode de résolution pour trouver une réfutation.
-- Retourne Nothing si aucune réfutation n'a pu être trouvée, retourne la
-- réfutation utilisée pour dériver [] sinon.
resolve :: [Clause] -> Maybe Derivative
resolve cnf =
    let initMap = M.fromList [ (c, InitialClause c) | c <- cnf ]
    in closure initMap initMap
  where
    -- Etant donné les clauses déjà dérivées et les clauses venant d'être
    -- dérivées à l'étape précédente, retourne éventuellement l'arbre de
    -- dérivation de la clause vide (derivs est un sous-enseble de clauses).
    -- Les deux ensembles de clauses sont sous la forme d'un mapping clause ->
    -- dérivée pour pouvoir rechercher rapidement si une clause existe (deux
    -- clauses identiques pourraient exister sous la forme de deux dérivées
    -- différentes).
    closure :: M.Map Clause Derivative -> M.Map Clause Derivative
            -> Maybe Derivative
    closure clauses derivs
        | refut@(Just _) <- M.lookup emptyClause derivs =
            -- La clause vide a été trouvée dans les dernières dérivées.
            refut
        | M.null (derivs) =
            -- Aucune dérivée à l'étape précédente, impossible de dériver [].
            Nothing
        | otherwise =
            -- Tente une étape supplémentaire de fermeture en dérivant de
            -- nouvelles clauses des dernières dérivées trouvées.
            closure clauses' derivs'
      where
        -- Tente de créer de nouvelles dérivées en combinant les dérivées vanant
        -- d'être trouvées avec toutes les clauses trouvées au cours de la
        -- fermeture.
        derivs' = M.fromList [
              (c', Derivative c' (d1, d2))
            | (c1, d1) <- M.toList derivs
            , (c2, d2) <- M.toList clauses
              -- Évite les dérivées réciproques en appliquant la règle de
              -- dérivation que sur lit1 = p et lit2 = ~p :
            , lit1@(Lit lit1Name) <- S.toList c1
            , let lit2 = OppLit lit1Name
            , lit2 `S.member` c2
              -- Applique la règle de distributivité sur les clauses c1 & c2 :
            , let c' = S.delete lit1 c1 `S.union` S.delete lit2 c2
              -- Ignore les clauses déjà dérivées :
            , c' `M.notMember` clauses
              -- Ignore les clauses valides comme celles-ci ne sauraient pas
              -- dériver la clause vide :
            , not (isValid c')
            ]

        -- L'opération d'union des deux Maps s'exécute en temps linéaire.
        clauses' = M.union clauses derivs'

-- Retourne True si la clause c contient une paire complémentaire.
-- Complexité : O(n * log n) où n est le nombre de littéraux de c.
isValid :: Clause -> Bool
isValid c = any ((`S.member` c) . complement) (S.toList c)

main :: IO Int
main = forever $ do
    eFormula <- runParser "stdin" <$> T.getLine
    case eFormula of
        Left err  -> hPrint stderr err
        Right formula -> do
            putStr "Formule parsée:\n\t"
            case formula of
                Just ast -> do
                    print ast

                    let simplified = simplify ast
                    let clauses    = normalize simplified

                    putStr "Formule simplifiée:\n\t"
                    print simplified

                    putStrLn "Clauses:"
                    forM_ clauses $ \clause -> do
                        putStr "\t"
                        putStrLn (intercalate " | " (map show (S.toList clause)))

                    putStr "Conclusion:\n\t"
                    case resolve clauses of
                        Just _  -> putStrLn "Formule inconsistante"
                        Nothing -> putStrLn "Formule consistante"

                Nothing  -> do
                    putStrLn "Formule vide"

                    putStr "Conclusion:\n\t"
                    putStrLn "Formule consistante"
