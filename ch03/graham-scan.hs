import Data.List

data Direction = TurnLeft | TurnRight | Straight deriving (Show, Eq)
data Point = Point Int Int deriving (Show, Eq)

ccw :: Point -> Point -> Point -> Direction
ccw (Point x1 y1) (Point x2 y2) (Point x3 y3)
    | exp > 0 = TurnLeft
    | exp < 0 = TurnRight
    | otherwise = Straight
  where exp = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

ccws :: [Point] -> [Direction]
ccws ([]) = []
ccws (_:[]) = []
ccws (_:_:[]) = []
ccws (a:b:c:ps) =
    ccw a b c : ccws (b:c:ps)

comparePolarWith :: Point -> Point -> Point -> Ordering
comparePolarWith (Point x y) (Point x1 y1) (Point x2 y2) =
    let c = compare (atan2 (fromIntegral (y1 - y)) (fromIntegral (x1 - x))) (atan2 (fromIntegral (y2 - y)) (fromIntegral (x2 - x)))
    in if c == EQ
          then compare x1 x2
          else c

scan' :: [Point] -> [Point]
scan' l@(p:ps)
    | r /= l = scan' r
    | otherwise = l
  where rst =  map (\(p, _) -> p) $ filter (\(_, d) -> d /= TurnRight) $ zip ps (ccws (l ++ [p]))
        r = p:rst

scan'' :: [Point] -> [Point] -> ([Point], [Point])
scan'' [] l@(c:b:a:ps)
    | ccw a b c == TurnRight = scan'' [] (c:a:ps)
    | otherwise = ([], l)
scan'' (q:qs) l
    | length l < 3 = scan'' qs (q:l)
scan'' (q:qs) l@(_:[]) = ([], q:l)
scan'' (q:qs) l@(c:b:a:ps)
    | ccw a b c == TurnRight = scan'' (q:qs) (c:a:ps)
    | otherwise = scan'' qs (q:l)

scan :: [Point] -> [Point]
scan ps =
    let lowest = minimumBy (\(Point x1 y1) (Point x2 y2) ->
                                let cy = compare y1 y2
                                in if cy == EQ
                                      then compare x1 x2
                                      else cy) ps
        comparePolar = comparePolarWith lowest
        sortedPoints = sortBy comparePolar ps
        (_, result) = scan'' (sortedPoints ++ [lowest]) []
    in result
