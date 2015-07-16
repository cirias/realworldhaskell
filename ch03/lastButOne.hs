-- file: ch03/lastButOne.hs
lastButOne :: [a] -> [a]
lastButOne xs = if null xs
                then xs
                else take (length xs - 1) xs
