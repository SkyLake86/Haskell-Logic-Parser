
newtype Parser a = P (String -> [(a, String)])