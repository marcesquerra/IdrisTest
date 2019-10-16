module Utils

public export
(*) : String -> Nat -> String
(*) str Z = ""
(*) str (S n) = str ++ (str * n)

