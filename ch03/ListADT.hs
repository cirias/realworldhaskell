-- file: ch03/ListADT.hs
data List a = Cons a (List a)
            | Nil
              deriving (Show)

reverseFromList (Cons x xs) = x:(reverseFromList xs)
reverseFromList Nil = []
