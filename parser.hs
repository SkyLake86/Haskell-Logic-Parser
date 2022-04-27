{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Eta reduce" #-}

import ParsingHelpers


type Bracket k v = [(k, v)]
type Substitute = Bracket String Bool

data Formula = Val Bool
            | Var String
            | Not Formula
            | And Formula Formula
            | Or Formula Formula
            | Imply Formula Formula
            | Iff Formula Formula deriving (Show, Eq)

eval :: Substitute -> Formula -> Bool
eval _ (Val b) = b
eval ks (Var p) = find p ks
eval ks (Not p) = not (eval ks p)
eval ks (And p q) = (eval ks p) && (eval ks q)
eval ks (Or p q) = (eval ks p) || (eval ks q)
eval ks (Imply p q) = (eval ks p) ==> (eval ks q)
eval ks (Iff p q) = (eval ks p) == (eval ks q)

printFormula :: Formula -> [Char]
printFormula (Val b) = show b
printFormula (Var p) = p
printFormula (Not p) = case p of
                        Val b -> "~" ++ show b
                        Var a -> "~" ++ a
                        _ -> "~(" ++ printFormula p ++ ")"
printFormula (And p q) = case p of
                        (And a b) -> "(" ++ printFormula a ++ " & " ++ printFormula b ++ " & " ++ printFormula q ++ ")"
                        _ -> case q of
                            (And c d) -> "(" ++ printFormula p ++ " & " ++ printFormula c ++ " & " ++ printFormula d ++ ")"
                            _ -> "(" ++ printFormula p ++ " & " ++ printFormula q ++ ")"
printFormula (Or p q) = case p of
                        (And a b) -> "(" ++ printFormula a ++ " V " ++ printFormula b ++ " V " ++ printFormula q ++ ")"
                        _ -> case q of
                            (And c d) -> "(" ++ printFormula p ++ " V " ++ printFormula c ++ " V " ++ printFormula d ++ ")"
                            _ -> "(" ++ printFormula p ++ " V " ++ printFormula q ++ ")"
printFormula (Imply p q) = "(" ++ printFormula p ++ " ==> " ++ printFormula q ++ ")"
printFormula (Iff p q) = "(" ++ printFormula p ++ " <==> " ++ printFormula q ++ ")"

for1 = Imply (And (Not (Val True)) (Val True)) (Iff (Val True) (Val False))
for2 = And (Val True) (And (Val False) (Not(Val True)))
for3 = Imply (And (Var "A") (Imply (Var "A") (Var "B"))) (Var "B")


(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

find :: Eq k => k -> Bracket k v -> v
find k ls = head [v | (p, v) <- ls, p == k]


