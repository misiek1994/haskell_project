-- main = do
--     putStrLn "Hello, World!"
    
numbers = [1, 2, 3, 4]
vec = [1, 0, 1, 0]
list_of_lists = [[1, 2, 3, 4], [1, 2, 3, 5], [1, 1, 1, 3], [2, 3, 3, 4]]
list_of_lists_of_lists = [[[1, 2, 3, 4], [1, 2, 3, 5], [1, 1, 1, 3], [2, 3, 3, 4]], 
                          [[1, 2, 3, 4], [1, 2, 3, 5], [1, 1, 1, 2], [2, 3, 3, 5]]]

map_length = 10
map_height = 10
                          
                          
distance [] [] = 0
distance a b = (head a - head b)^2 + (distance (tail a) (tail b))



newtype Point x y val = Point (Integer, Integer, Integer)  deriving (Eq,Show)
instance (Num x, Num y, Num val)  => Num (Point x y val) where
   Point (x1,y1,val1) + Point (x2,y2,val2) = Point ((mod (x1+x2) map_length), (mod (y1+y2) map_height) ,val1+val2)
instance (Ord x, Ord y, Ord val) => Ord (Point x y val) where
   Point (x1,y1,val1) < Point (x2,y2,val2) = val1<val2
   


add_touples :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
add_touples a b =  (fst a + fst b, snd a + snd b)

compare_touples :: (Integer, Integer) -> (Integer, Integer) -> Bool
compare_touples a b = snd a < snd b

-- min_distance a [] = (-1, 1000000000000000)
-- min_distance a [b] = (0, distance a b)
--min_distance a b | (compare_touples (min_distance a [head b]) (min_distance a (tail b))) = min_distance a [head b]
--                 | otherwise = (add_touples (1, 0) (min_distance a (tail b)))

-- min_distance_2d a [] = (-1, -1, 1000000000000)                 
-- min_distance_2d a [b] = (0, fst (min_distance a b), snd (min_distance a b))
-- min_distance_2d a b | (0, fst (min_distance_2d a (head b)), snd (min_distance a (head b)))


min_distance_in_column a [] = Point (-1, -1, 1000000000000000)
min_distance_in_column a [b] = Point (0, 0, distance a b)
min_distance_in_column a b | ((min_distance_in_column a [head b]) < (min_distance_in_column a (tail b))) = min_distance_in_column a [head b]
                           | otherwise = (Point (0, 1, 0) + (min_distance_in_column a (tail b)))
