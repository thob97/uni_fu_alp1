import Data.Char

primzahlen :: [Int] --Aus Vorlesung
primzahlen = sieb [2..]
    where
        sieb (x:xs) = x : sieb [n | n<- xs, mod n x > 0]


--Aufgabe 3
schwacheGoldbachTripel :: Int -> [(Int,Int,Int)]
schwacheGoldbachTripel x | (x<=5)= error "Muss größer als 5 sein" 
         | (mod x 2)==0=error"Muss Ungnade sein"
         | otherwise = [(a,b,c)| a<-(take x primzahlen),b<-(take x primzahlen),c<-(take x primzahlen), (a+b+c)==x, a<=b, b<=c]

testGoldenBachVermutung n 
    | n<=5 = error "Muss groeßer gleich 5 sein"
    | n == 6 = False
    | n==7 = True
    | mod n 2 == 0 = testGoldenBachVermutung (n-1)
    | length (schwacheGoldbachTripel n) >= 1 = testGoldenBachVermutung (n-1)
    | otherwise = False

--Aufgabe 4

plusPlus :: [a]-> [a] -> [a]
plusPlus xs ys = foldr (:) ys xs
    

--Aufgabe 5 
machGross :: String -> String
machGross xs = foldl help [] xs
    where
        help a b = a ++ [(toUpper b)]

