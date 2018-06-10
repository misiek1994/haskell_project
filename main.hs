import System.Random
import System.IO
import Data.List.Split
--import Control.Monad

-- test data
numbers = [1, 3, 4] :: [Float]
vec = [1, 1, 0] :: [Float]
list_of_lists = [[1, 3, 4], [1, 3, 5], [1, 1, 3], [2, 3, 4]] :: [[Float]]
list_of_lists_of_lists = [[[1, 3, 4], [1, 3, 5], [1, 1, 3], [2, 3, 4]], 
                          [[1, 3, 4], [1, 3, 5], [1, 1, 2], [2, 3, 5]]] :: [[[Float]]]
--end of test data
                          
                          
map_length = 10
map_height = 10
single_vec_size = 3
                          
diffrence :: [Float] -> [Float] -> Float                          
diffrence [] [] = 0
diffrence a b = (head a - head b)^2 + (diffrence (tail a) (tail b))

newtype Point x y val = Point (Int, Int, Float)  deriving (Eq,Show)
instance (Num x, Num y, Num val)  => Num (Point x y val) where
   Point (x1,y1,val1) + Point (x2,y2,val2) = Point ((mod (x1+x2) map_length), (mod (y1+y2) map_height) ,val1+val2)
instance (Ord x, Ord y, Ord val) => Ord (Point x y val) where
   Point (x1,y1,val1) < Point (x2,y2,val2) = val1<val2   

min_diffrence_in_row a [] = Point (-1, -1, 1000000000000000)
min_diffrence_in_row a [b] = Point (0, 0, diffrence a b)
min_diffrence_in_row a b | ((min_diffrence_in_row a [head b]) < (min_diffrence_in_row a (tail b))) = min_diffrence_in_row a [head b]
                           | otherwise = (Point (1, 0, 0) + (min_diffrence_in_row a (tail b)))
                           
min_diffrence a [] = Point (-1, -1, 1000000000000000)
min_diffrence a [b] = min_diffrence_in_row a b
min_diffrence a b | ((min_diffrence a [head b]) < (min_diffrence a (tail b))) = min_diffrence a [head b]
                 | otherwise = (Point (0, 1, 0) + (min_diffrence a (tail b)))

random_vec :: Int -> IO([Float])
random_vec 0 = return []
random_vec n = do
    r <- randomRIO (1,10) :: IO Float
    rs <- random_vec (n-1)
    return (r:rs) 

random_row :: Int -> Int -> IO([[Float]])
random_row 0 _ = return []
random_row n vec_size = do
    r <- random_vec vec_size
    rs <- random_row (n-1) vec_size
    return (r:rs) 
    
random_map :: Int -> Int -> Int -> IO([[[Float]]])
random_map 0 _ _ = return []
random_map n m vec_size = do
    r <- random_row m vec_size
    rs <- random_map (n-1) m vec_size
    return (r:rs) 
    
som_map = random_map map_length map_height single_vec_size  

-- IO operations

get_Float_vectors data_list | data_list == [] = []
                             | otherwise = ((map read (splitOneOf " " (head data_list)) :: [Float]):(get_Float_vectors (tail data_list)))
            
get_data_from_file = do
    content <- readFile "data_short.txt"
    let lines_list = lines content
    let res = get_Float_vectors lines_list
    return res
    
rgb_points = get_data_from_file   

 
write_som_to_file som = do
    file_handler <- openFile "output.txt" WriteMode
    hPrint file_handler som
    hClose file_handler

-- end of IO

get_min_deffrences :: [[Float]] -> [[[Float]]] -> [Point Int Int Float]
get_min_deffrences rgb_ som_ | rgb_ == [] = []
                             | otherwise = [min_diffrence (head rgb_) som_]++(get_min_deffrences (tail rgb_) som_)

                        

main = do
    som <- som_map
    rgb <- rgb_points
    let pure_result = get_min_deffrences rgb som
    print pure_result
    write_som_to_file som
    
    
    