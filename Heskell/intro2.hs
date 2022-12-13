index:: [Int] -> Int -> Int
index [] _ = error "leer"
index (x:xs) n
    |x == n = 0
    |otherwise = 1+ index xs n

kleinstes :: [Int] -> Int
kleinstes [] = error "leer"
kleinstes [x] = x
kleinstes (x:y:xs)
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