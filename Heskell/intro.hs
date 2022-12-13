mal2 :: Int -> Int
mal2 x = 2 * x

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci(x-1)+ fibonacci(x-2)

größeres :: Int -> Int -> Int
größeres a b
    | a < b = b
    | otherwise = a

erstes :: [Int] -> Int
erstes [] = -1
erstes (x:xs) = x

letztes :: [Int] -> Int
letztes [] = -1
letztes [x] = x
letztes (x:xs) = letztes xs

summe :: [Int] -> Int
summe [] = -1
summe (x:xs) = x + summe xs

länge :: [Int] -> Int
länge [] = 0
länge (x:xs) = länge xs + 1

nimm :: Int -> [Int] -> Int
nimm _ [] = -1
nimm 0 (x:xs) = x
nimm n (x:xs) nimm (n- 1) xs

maxi :: [Int] -> Int
maxi [] = error "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
maxi [x] = x
maxi (x:y:ys)
    | x < y = maxi (y:ys)
    | otherwise = größtes (x:ys)

enthalten :: [Int] -> Int -> Bool
enthalten [] n = False
enthalten (x:xs) n 
    | n == x = True
    | enthalten xs n

sortiert :: [Int] -> Bool
sortiert [] = False
sortiert [x] = True
sortiert (x:y:xs)
    | x<y<sortiert[xs] = True
    |otherwise = False


index:: [Int] -> Int -> Int
index [] _ = error "leer"
index [x] x = 0
index (x:xs) xs = index (xs-1)

anzahl :: [Int] -> Int -> Int
anzahl [] _ = 0
anzahl (x:xs) n
    |x == n = 1+ anzahl xs n
    |otherwise = anzahl xs n
    

--länge :: [Int] -> Int
--länge [] = 0
--länge (x:xs) = 1 + länge xs


--index :: [Int] -> Int -> Int
--index [] n = "leere liste"
--index [x] = 1
--index (x:y:xs) n
 --   | n == 1 = xs
   -- | n == 2 = y
    -- | otherwise = index xs

--löschen :: [Int] -> Int -> [Int]
--löschen [] n = error "leer"
--löschen (x:xs) n
   -- | n == x = xs
   -- | otherwise = x:löschen xs n

--umdrehen :: [Int] -> [Int]
--umdrehen [] = []
--umdrehen (x:xs) = umdrehen xs ++ [x]

entfernen :: [Int] -> Int -> [Int]
entfernen [] _ = error "leer" 
entfernen (x:xs) 0 = xs
entfernen (x:xs) n = x: entfernen xs (n-1)


