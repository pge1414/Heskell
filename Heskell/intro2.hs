index:: [Int] -> Int -> Int
index [] _ = error "leer"
index (x:xs) n
    |x == n = 0
    |otherwise = 1+ index xs n

kleinstes :: [Int] -> Int
kleinstes [] = error "leer"
kleinstes [x] = x
kleinstes (x:y:ys)
    | x <=  y = kleinstes (x:ys)
    | otherwise = kleinstes (y:ys)

anzahl :: [Int] -> Int -> Int
anzahl [] _ = 0
anzahl (x:xs) n
    |x == n = 1+ anzahl xs n
    |otherwise = anzahl xs n

sortiert :: [Int] -> Bool
sortiert [] = error "leer"
sortiert [x] = True
sortiert (x:y:ys)
    |x <= y = sortiert (y:ys)
    |otherwise = False

ersteN :: Int -> [Int]
ersteN 0 = []
ersteN n = ersteN(n-1) ++ [n]

sortieren :: [Int] -> [Int]
sortieren [] = []
sortieren (x:y:ys) 
    | x <= y = sortieren ys
    | otherwise = sortieren (x:ys)

umdrehen :: [Int] -> [Int]
umdrehen [] = []
umdrehen (x:xs) = umdrehen xs ++ [x]

löschen :: [Int] -> Int -> [Int]
löschen [] n = error "leer"
löschen (x:xs) n
    | n == x = xs
    | otherwise = x:löschen xs n

selectionSort :: [Int] -> [Int]
selectionSort [] = []
selectionSort xs = selectionSort (löschen xs k)
    where k = kleinstes xs

gerade :: Int -> Bool
gerade x
    | mod x 2 == 0 = True
    | otherwise = False

filterer :: (Int -> Bool) -> [Int] -> [Int]
filterer f [] = []
filterer f (x:xs) 
    | f x == True = x: filterer f xs
    | otherwise = filterer f xs

addieren :: Int -> Int -> Int 
addieren a b = a + b

zipper :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
zipper f [] [] = []
zipper f x [] = error "nicht gleich lang"
zipper f (x:xs) (y:ys) = f x y: zipper f xs ys

länge :: [Int] -> Int
länge [] = 0
länge (x:xs) = 1+ länge xs

größer :: Int -> Int -> Int
größer x y 
    | x > y = x
    | otherwise = y

folder :: (Int -> Int -> Int) -> Int -> [Int] -> Int
folder f n [] = error "leere Liste"
folder f n [x] = f n x
folder f n (x:y:ys) = folder f n (ys ++ [f x y])

insertion :: [Int] -> Int -> [Int]
insertion [] n = [n]
insertion (x:xs) n
    | n > x = x:insertion xs n
    | otherwise = n:x:xs

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insertion (insertionSort xs) x

bubble :: [Int] -> [Int]
bubble [] = []
bubble (x:y:ys)
    | x <= y = x:bubble (y:ys)
    | otherwise = y:x:ys

bubbleSort :: [Int] -> [Int]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort xs
    | sortiert (bubble xs) = bubble xs
    | otherwise = bubbleSort (bubble xs)

--merge :: [Int] -> [Int]
--merge [] = []
--merge [x] = [x]
--merge [xs] = merge ((length [xs])*0,5)

--mergeSort :: [Int] -> [Int]
--mergeSort [] = []
--mergeSort (x:xs) = merge (x:xs)

komprimieren :: [Char] -> [Char]
komprimieren [] = []
komprimieren [x] = [x]
komprimieren (x:y:ys)
    | x ==y = komprimieren (x:ys)
    | otherwise = komprimieren (x:y:ys)