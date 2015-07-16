-- file: reverseFromList.hs

reverseFromList (Cons x xs) = x:(reverseFromList xs)
reverseFromList Nil = []
