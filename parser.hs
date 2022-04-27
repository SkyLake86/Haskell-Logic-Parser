
newtype Parser a = P (String -> [(a, String)])

type Bracket k v = [(k, v)]

find :: Eq k => k -> Bracket k v -> v
find k ls = head [v | (p, v) <- ls, p == k]

data Formula = Value Bool
            | Var String
            | Not Formula
            | And Formula Formula
            | Or Formula Formula
            | Imply Formula Formula
            | Iff Formula Formula
