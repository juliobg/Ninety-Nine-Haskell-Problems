import System.Random
import qualified Data.Map as Map
import Data.List

myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "error"
myButLast [x] = error "error"
myButLast (xa:(xs:[])) = xa
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Integer -> a
elementAt [] n = error "error"
elementAt (xa:xs) 0 = xa
elementAt (_:xs) n = elementAt xs (n-1)

myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (xa:xs) = myReverse xs ++ [xa]

isPalindrome x = (x == myReverse x)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List(x:xs)) = (flatten x) ++ flatten(List(xs)) 

compress :: Eq a => [a] -> [a]
compress (x1:x@(x2:_)) = (if (x1 == x2) then [] else [x1]) ++ (compress x)
compress r = r

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x@(x1:_)) = (takeWhile (==x1) x) : pack (dropWhile (==x1) x)

encode :: Eq a => [a] -> [(Int, a)]
encode l = map (\x -> (length x, head x)) (pack l)

data RLEElement a = Single a | Multiple (Int,a) deriving (Show)
encodeModified :: Eq a => [a] -> [RLEElement a]
encodeModified l = map (\x -> if (length x == 1) then Single (head x) else Multiple (length x, head x)) (pack l)

decodeModified :: [RLEElement a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = case x of 
                    Single v -> v : (decodeModified xs)
                    Multiple (n, v) -> (replicate n v) ++ decodeModified xs 

decode :: [(Int, a)] -> [a]
decode [] = []
decode ((n, v):xs) = (replicate n v) ++ decode xs 

dupli = concatMap (replicate 2) 

repli x n = concatMap (replicate n) x

dropEvery xs n = [i | (i,j) <- (zip xs (myCycle n)), j /= n] where myCycle n = [1..n] ++ myCycle n

split xs n = ([i | (i,j) <- zipped, j <= n ], [i | (i,j) <- zipped, j > n]) where zipped = zip xs [1..] 

slice xs n1 n2 = [i | (i,j) <- zipped, j >= n1, j <= n2] where zipped = zip xs [1..] 

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xs) n = if (n > 0) then rotate (xs ++ [x]) (nn - 1) else rotate (x:xs) (length (x:xs) + nn) where nn = n `mod` length (x:xs)

removeAt xs n = (xs !! (n-1), [i | (i,j) <- zipped, j /= n]) where zipped = zip xs [1..] 

insertAt c xs 1 = c:xs
insertAt c (x:xs) n = x : (insertAt c xs (n-1)) 

range n1 n2 
  | n1 > n2 = reverse (range n2 n1)
  | n1 == n2 = [n1]
  | otherwise = n1:(range (n1+1) n2)

rndSelect l n = do
  g <- getStdGen
  return (map (l !! ) (take n (randomRs (0, length l - 1) g)))
 
diffSelect' 0 m (l, dict) = return (l, dict)
diffSelect' n m (l, dict) = do
  r <- (randomRIO (0, m))
  d <- ((diffSelect' (n-1) (m-1) ((get r dict):l, (Map.insert r (get m dict) dict))))
  return d
  where get k dict = case (Map.lookup k dict) of 
                       Just value -> value 
                       Nothing -> k
 
diffSelect n m = do
    (l, dict) <- (diffSelect' n m ([], Map.empty))
    return l

permutation l = do
  p <- (diffSelect (length l) ((length l) - 1))
  return (map (l !! ) p)

combination 0 _ = [[]]
combination n [] = []
combination n (x:xs) = [ x:t | t <- (combination (n-1) xs)] ++ combination n xs

comb 0 (t, l) = [([], t++l)]
comb n ([], l) = []
comb n ((x:xs), l) = [ (x:u, v) | (u, v) <- (comb (n-1) (xs, l)) ] ++ comb n (xs, x:l)

-- group [] _ = [[]]
-- group (x:xs) l = [ u:w | (u, v) <- (comb x (l, [])), w <- (group xs v) ]

lsort = sortBy (\x y -> compare (length x) (length y))

lfsort = concat . lsort . groupBy (\x y -> (length x) == (length y)) . lsort 
