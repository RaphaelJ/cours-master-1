{-# LANGUAGE OverloadedStrings, PatternGuards #-}

import Control.Applicative ((<$>), (<|>))
import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.List (intercalate, partition)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.IO (
      Handle, IOMode (WriteMode), hClose, hPrint, hPutStr, hPutStrLn, openFile
    , stderr
    )
import Text.Parsec (
      ParseError, SourceName, between, eof, many, parse, string, try
    )
import Text.Parsec.Char (alphaNum, char, lower)
import Text.Parsec.Text (Parser)
import Text.Printf (printf, hPrintf)

-- Types -----------------------------------------------------------------------

-- Une formule est soit une formule vide, soit un arbre d'expression (Expr).
data Formula = EmptyFormula
             | Formula      Expr

instance Show Formula where
    show (Formula expr) = show expr
    show EmptyFormula   = "Formule vide"

-- Contient l'arbre syntaxique d'une formule non vide.
-- Les opérateurs =>, <=,<=> et <~> sont convertis en disjonctions et
-- conjonctions au parsing et n'existent donc pas dans l'arbre syntaxique.
data Expr = Val Bool
          | Var Text
          | Not Expr
          | And Expr Expr
          | Or  Expr Expr
    deriving (Eq, Ord)

-- Permet d'afficher une formule non vide.
-- Évite les parenthèses inutiles. Par exemple, "(a | b) | c" devient
-- "a | b | c".
instance Show Expr where
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
type Clause = S.Set Litteral -- 'type' définit un type "alias", équivalent à
                             -- 'typedef' en C.

emptyClause :: Clause
emptyClause = S.empty

-- Affiche une clause en séparant les littéraux par le caractère "|".
showClause :: Clause -> String
showClause c | S.null c  = "[]"
             | otherwise = intercalate " | " (map show (S.toList c))

-- Contient les deux clauses dérivées à partir desquelles une dérivée a été
-- créée s'il ne s'agit pas d'une clause de la CNF, de telle manière à former
-- un arbre permettant de reconstituer la réfutation.
data Derivative = InitialClause Clause
                | Derivative    Clause (Derivative, Derivative)
    deriving (Eq, Ord)

-- Parseur ---------------------------------------------------------------------

-- Tente de parseur le fichier nommé 'filename' (utilisé dans les messages
-- d'erreur) ayant 'txt' comme contenu.
-- Retourne soit une formule, soit une erreur de parsing.
runParser :: SourceName -> Text -> Either ParseError Formula
runParser filename txt =
    parse formula filename noSpaces
  where
    -- Supprime les espaces avant de parser.
    noSpaces = T.filter (not . isSpace) txt

    formula :: Parser Formula
    formula = do
        formul <- (    (Formula <$> expr)
                   <|> return EmptyFormula)
        eof
        return formul

    -- Implémente la priorité des opérateurs avec une descente récursive.

    expr, orExpr, andExpr, atom, variable :: Parser Expr
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

-- Simplifie une formule en faisant remonter ses 'true'/'false' à la racine de
-- l'arbre.
-- Il est aisé de prouver par induction que cet algorithme génère un arbre où
-- seule la racine peut contenir 'true' ou 'false', ceux se trouvant plus bas
-- dans l'arbre étant systématiquement et récursivement soit simplifiées, soit
-- remontées au niveau supérieur.
simplify :: Formula -> Formula
simplify EmptyFormula   = EmptyFormula
simplify (Formula expr) = Formula (simplifyExpr expr)
  where 
    simplifyExpr (Not p) | Val True  <- p' = Val False
                         | Val False <- p' = Val True
                         | Not q     <- p' = q
                         | otherwise       = Not p'
      where
        p' = simplifyExpr p
    simplifyExpr (And p q) | Val True  <- p' = q'
                           | Val False <- p' = Val False
                           | Val True  <- q' = p'
                           | Val False <- q' = Val False
                           | otherwise       = And p' q'
      where
        p' = simplifyExpr p
        q' = simplifyExpr q
    simplifyExpr (Or p q) | Val True  <- p' = Val True
                          | Val False <- p' = q'
                          | Val True  <- q' = Val True
                          | Val False <- q' = p'
                          | otherwise       = Or p' q'
      where
        p' = simplifyExpr p
        q' = simplifyExpr q
    simplifyExpr p = p

-- Retourne la formule en CNF.
-- La formule en entrée doit avoir été simplifiée (elle ne doit plus contenir de
-- 'true'/'false' en dehors de sa racine).
normalize :: Formula -> [Clause]
normalize EmptyFormula          = []
normalize (Formula (Val True))  = []
normalize (Formula (Val False)) = [emptyClause]
normalize (Formula formula)     =
    removeInclusive (
    unitPropagation (
    removeValid (
    clauses (
    distribute (
    deMorgan formula)))))
  where
    -- Applique les règles de De Morgan pour propager les négations vers les
    -- feuilles de l'arbre syntaxique. Supprime aussi les doubles négations.
    -- Il est aisé de prouver par induction qu'après l'exécution de cet
    -- algorithme, plus aucune négation ne peut porter sur une conjonction ou
    -- disjonction.
    -- Complexité : O(n) où n est le nombre de littéraux de la formule.
    deMorgan (Not (Not p))   = deMorgan p
    deMorgan (Not (And p q)) = Or  (deMorgan (Not p)) (deMorgan (Not q))
    deMorgan (Not (Or  p q)) = And (deMorgan (Not p)) (deMorgan (Not q))
    deMorgan (Not p)         = Not (deMorgan p)
    deMorgan (And p q)       = And (deMorgan p) (deMorgan q)
    deMorgan (Or  p q)       = Or  (deMorgan p) (deMorgan q)
    deMorgan p               = p

    -- Applique les règles de distributivité pour remonter les conjonctions de
    -- la formule.
    -- Les négations de la formule à distribuer ne peuvent porter que sur des
    -- littéraux (comme garantit par l'exécution de l'algorithme de De Morgan).
    distribute p = fst (distributeCached M.empty p)

    -- Applique les règles de distributivité en commençant en profondeur pour
    -- faire remonter les conjonctions.
    -- Accepte un cache mappant les formules non distribuées vers leur
    -- équivalent CNF (algorithme dynamique), retourne l'expression en CNF et
    -- le cache mis à jour.
    -- Les négations de la formule à distribuer ne peuvent porter que sur des
    -- littéraux (comme garantit par l'exécution de l'algorithme de De Morgan).
    distributeCached :: M.Map Expr Expr -> Expr -> (Expr, M.Map Expr Expr)
    distributeCached cache p | Just cached <- M.lookup p cache =
        -- Expression en cache, déjà traitée.
        (cached, cache)
    distributeCached cache r@(Or  p q) =
        let (p', cache')  = distributeCached cache  p
            (q', cache'') = distributeCached cache' q
            r' = distributeDisj (Or p' q')
        in (r', M.insert r r' cache'')
      where
        -- Distribue récursivement une disjonction dont les sous-expressions
        -- sont en CNF. Retourne une fonction en CNF.
        distributeDisj (Or (And s t) e) =
            -- Invariant : { s, t, u } sont en CNF.
            And (distributeDisj (Or s e)) (distributeDisj (Or t e))
        distributeDisj (Or s (And t u)) =
            -- Invariant : { s, t, u } sont en CNF.
            And (distributeDisj (Or s t)) (distributeDisj (Or s u))
        distributeDisj s                = s -- s est déjà en CNF.
    distributeCached cache r@(And p q) =
        let (p', cache')  = distributeCached cache  p
            (q', cache'') = distributeCached cache' q
            r' = And p' q'
        in (r', M.insert r r' cache'')
    distributeCached cache (Not p) = (Not p, cache) -- p est un littéral
    distributeCached cache p       = (p, cache)

    -- Extrait les clauses d'un arbre syntaxique en CNF à l'aide d'un parcours
    -- en profondeur.
    clauses :: Expr -> [Clause]
    clauses =
        goConj []
      where
        -- Ajoute un nouvel ensemble de littéraux pour chaque terme de chaque
        -- conjonction.
        goConj acc (And p q) = goConj (goConj acc q) p
        goConj acc p         = goDisj (S.empty) p : acc

        -- Rajoute tous les littéraux des sous-disjonctions à l'ensemble des
        -- littéraux de la clause en cours de parcours.
        goDisj clause (Or p q)          = goDisj (goDisj clause p) q
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
        -- Parcourt les clauses du second argument et rajoute celles qui ne sont
        -- pas redondantes dans 'acc'.
        go acc []       = acc
        go acc (c : cs) |    any (`S.isSubsetOf` c) acc
                          || any (`S.isSubsetOf` c) cs  = go acc       cs
                        | otherwise                     = go (c : acc) cs

    -- Recherche les clauses ne contenant qu'un seul littéral (clauses unitaire)
    -- et supprime le complément de ce littéral dans toutes les clauses où il se
    -- trouve (le complément ne saurait être satisfaisable en même temps que la
    -- clause unitaire). Ré-itère l'opération si de nouvelles clauses unitaires
    -- ont été créées de cette manière.
    -- La clause vide peut être créée si tous les littéraux d'une clauses on été
    -- supprimés. Cette fonction peut retourner certaines clauses en plusieurs
    -- exemplaires.
    -- Complexité : O(n² * log m) où n est le nombre de clauses et m le nombre
    -- de littéraux par clause.
    unitPropagation :: [Clause] -> [Clause]
    unitPropagation cnf =
        let (units, nonUnits) = partition isUnit cnf
        in go units [] nonUnits
      where
        -- Parcourt les clauses unitaires du premier argument et les propages
        -- dans 'accUs' et 'cs' ('accUs' contient les clauses unitaires qu'il
        -- ne faut pas parcourir et 'cs' les clauses non unitaires).
        -- Retourne l'ensemble des clauses une fois la propagation effectuée.
        go []       accUs cs = accUs ++ cs
        go (u : us) accUs cs
            | any (compl `S.member`) us || any (compl `S.member`) accUs =
                -- Une seconde clause unitaire contient le complément du
                -- littéral u. On peut donc simplifier la clause, et l'ensemble
                -- de la formule, en un clause unitaire :
                [emptyClause]
            | otherwise                                                 =
                -- Relance le parcours avec les nouvelles clauses unitaires
                -- générées et celles qui n'ont pas encore été propagées.
                go (us' ++ us) (u : accUs) cs'
           where
            [lit] = S.toList u
            compl = complement lit

            (us', cs') = partition isUnit [ S.delete compl c | c <- cs ]

        isUnit c | S.size c == 1 = True
                 | otherwise     = False

-- Applique la méthode de résolution pour trouver une réfutation.
-- Retourne Nothing si aucune réfutation n'a pu être trouvée, retourne la
-- réfutation utilisée pour dériver [] sinon.
resolve :: [Clause] -> Maybe Derivative
resolve cnf =
    let initMap = M.fromList [ (c, InitialClause c) | c <- cnf
                             , not (isPureClause c) ]
    in closure initMap initMap
  where
    -- Une clause pure est une clause qui contient un littéral dont le
    -- complément n'est contenu dans aucune clause.
    -- Une telle clause est toujours satisfaisable et de ce fait, aucune de ses
    -- dérivées n'aboutira à la clause vide.
    isPureClause c = any (`S.member` c) pureLitterals

    -- Littéraux n'avant pas de complément dans aucune clause.
    pureLitterals =
        let litteralsSet = S.unions cnf
        in [ lit | lit <- S.toList litteralsSet
                 , not (complement lit `S.member` litteralsSet) ]

    -- Étant donné les clauses déjà dérivées et les clauses venant d'être
    -- dérivées à l'étape précédente, retourne éventuellement l'arbre de
    -- dérivation de la clause vide (derivs est un sous-ensemble de clauses).
    -- Les deux ensembles de clauses sont sous la forme d'un mapping clause ->
    -- dérivée pour pouvoir rechercher rapidement si une clause existe (deux
    -- clauses identiques pourraient exister sous la forme de deux dérivées
    -- différentes et on ne peut donc pas utiliser Set).
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
        -- Tente de créer de nouvelles dérivées en combinant les dérivées venant
        -- d'être trouvées avec toutes les clauses trouvées au cours de la
        -- fermeture. M.fromList va élimiter les dérivées qui ont la même
        -- clause.
        derivs' = M.fromList [
              (c', Derivative c' (d1, d2))
            | (c1, d1) <- M.toList derivs, (c2, d2) <- M.toList clauses
            , lit1 <- S.toList c1
            , let lit2 = complement lit1
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

-- I/O -------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs

    case args of
        [input, output] -> withDebug    input output
        [input]         -> withoutDebug input
        _               -> usage
  where
    -- Résous la formule du fichier 'input'. Affiche true si la formule est
    -- consistante, false sinon. Écrit les étapes de la résolution dans le
    -- fichier 'output'.
    withDebug input output =
        parseAndResolve input debugFct
      where
        -- Écrit les étapes de la résolution dans le fichier 'output'.
        debugFct formula simplified clauses solution = do
            h <- openFile output WriteMode

            hPutStr h "Formule interprétée:\n\t"
            hPrint h formula

            hPutStr h "Formule simplifiée:\n\t"
            hPrint h simplified

            hPutStrLn h "Clauses:"
            let clausesWithNum = zip clauses [(1 :: Int)..]
            forM_ clausesWithNum $ \(clause, i) ->
                hPrintf h "\t%d.\t%s\n" i (showClause clause)

            hPutStrLn h "Réfutation:"
            case solution of
                Just refut -> do
                    let clausesNums = M.fromList clausesWithNum
                    _ <- printRefut h clausesNums refut
                    return ()
                Nothing    -> hPutStrLn h "\tAucune réfutation trouvée."

            hPutStr h "Conclusion:\n\t"
            case solution of
                Just _  -> hPutStrLn h "Formule inconsistante."
                Nothing -> hPutStrLn h "Formule consistante."

            hClose h

        -- Effectue une descente récursive pour afficher les clauses de toutes
        -- les dérivées parentes d'une dérivée.
        -- Accepte en argument une Map contenant les numéros des clauses déjà
        -- affichées et la dérivée à explorer.
        -- Retourne une nouvelle Map où les dérivées parentes ont été ajoutées.
        printRefut :: Handle -> M.Map Clause Int -> Derivative
                   -> IO (M.Map Clause Int, Int)
        printRefut _ clausesNums (InitialClause c) =
            return (clausesNums, clausesNums M.! c)
        printRefut h clausesNums (Derivative c (d1, d2))
            | Just num <- c `M.lookup` clausesNums =
                return (clausesNums, num)
            | otherwise                            = do
                (clausesNums',  d1Num) <- printRefut h clausesNums  d1
                (clausesNums'', d2Num) <- printRefut h clausesNums' d2
                let thisNum        = M.size clausesNums'' + 1
                    clausesNums''' = M.insert c thisNum clausesNums''

                _ <- hPrintf h "\t%d.\t%s (%d, %d)\n" thisNum (showClause c)
                                                      d1Num d2Num

                return (clausesNums''', thisNum)

    -- Résous la formule du fichier 'input'. Affiche true si la formule est
    -- consistante, false sinon.
    withoutDebug input =
        parseAndResolve input debugFct
      where
        debugFct _ _ _ _ = return ()

    -- Parse et résous une formule se trouvant dans un fichier. Affiche true
    -- sur stdin si la formule est consistante, false sinon.
    -- Parse la formule contenue dans le fichier dont le nom a été donné en
    -- argument et accepte une fonction qui sera exécutée avec les données de
    -- résolution pour afficher des informations supplémentaires.
    parseAndResolve input debugFct = do
        content <- T.readFile input

        case runParser input content of
            Left err -> -- Erreur de parsing.
                hPrint stderr err
            Right formula -> do
                let simplified = simplify formula
                    clauses    = normalize simplified
                    solution   = resolve clauses

                case solution of
                    Nothing -> putStrLn "true"
                    Just _  -> putStrLn "false"

                debugFct formula simplified clauses solution

    usage = do
        putStrLn "Usage:"
        putStrLn "resolution <input_file> [<debug_file>]"
