import Data.List
--Aufgabe 1
length :: [a] -> Int
length [] = 0
length xs = helper xs 0
    where
        helper [] a = a
        helper (x:xs) a = helper xs (a+1)
        

f :: Int -> Int       
f 0 = 0
f n = helper n 0 0
    where
        helper n m a | n==m = a 
                     | otherwise = helper n m2 (m2*a+(m2-5))
                         where 
                             m2 = m+2

--Aufgabe 3
type Mobile = Int
type Cable = Int

match :: [Mobile] -> [Cable] -> [(Mobile,Cable)]
match _ [] = []
match [] _ = []
match ys (x:xs) = match [y | y<-ys, x<y] xs ++ [(x,y) | y<-ys, x==y] ++ match [y | y<-ys, x>y] xs

--Aufgabe 5 a)
isSorted :: (a->a->Bool) -> [a] -> Bool
isSorted f [] = True
isSorted f (x:[]) = True
isSorted f (x:y:xs) | f x y = isSorted f (y:xs) 
                    | otherwise = False

--b)
selectionsort :: Eq t => (t -> t -> Bool) -> [t] -> [t]
selectionsort f [] = []
selectionsort f xs = m:selectionsort f tail
   where 
     m = min f xs -- or use minimum from Data.List
     tail = delete m xs
     min f [] = error "Empty list"
     min f (x:xs) = min' f x xs 
          where
            min' f m [] = m
            min' f m (x:xs) | f x m     = min' f x xs
                            | otherwise = min' f m xs

insertionsort :: (t -> t -> Bool) -> [t] -> [t]
insertionsort f []     = []
insertionsort f (x:xs) = insert f x (insertionsort f xs)
    where
        insert f x [] = [x]
        insert f x (y:ys) | (f x y) = (x:y:ys)
                          | otherwise = y:insert f x ys

bubble f []  = []
bubble f [x] = [x]
bubble f (x:y:xs)
        | (f x y)==False     = y:bubble f (x:xs)
        | otherwise = x:bubble f (y:xs)

bubblesort :: (t -> t -> Bool) -> [t] -> [t]
bubblesort f [] = []
bubblesort f xs = bubblesort f (init ys) ++ [last ys]
   where ys = bubble f xs

quicksort :: (t -> t -> Bool) -> [t] -> [t]
quicksort f [] = []
quicksort f (x:xs) = quicksort f [y | y <- xs, f y x] ++ [x] ++ quicksort f [y | y <- xs, (f y x)==False]

--c)

testSortierAlg :: ((a -> a -> Bool) -> t -> [a]) -> (a -> a -> Bool) -> t -> Bool
testSortierAlg sort f xs = isSorted f (sort f xs)

--Aufgabe 6
fibList :: Int -> [Int]
fibList n = fib 0 0 n
    where
        fib p v 0 = []
        fib 0 v n = 1: fib 1 v (n-1)
        fib p v n = new: fib new p (n-1)
            where
                new = p+v