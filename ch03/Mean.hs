mean :: [Float] -> Float
mean [] = 0
mean xs = sum xs / (fromIntegral $ length xs)
