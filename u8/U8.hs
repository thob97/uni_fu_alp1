import Data.Char
import Data.List
-- AUFGABE 1
strToFloat (x:xs)| x=='-' = -1* helper (reverse(takeWhile (/='.') xs)) (drop 1 (dropWhile (/='.') xs)) 1 10
                  | x=='+' = helper (reverse(takeWhile (/='.') xs)) (drop 1 (dropWhile (/='.') xs)) 1 10
                  | otherwise = helper (reverse(takeWhile (/='.')(x:xs))) (drop 1 (dropWhile (/='.') (x:xs))) 1 10
                   where
                     helper [] [] a b = 0
                     helper [] (y:ys) a b = (fromIntegral(digitToInt y))/b + helper [] ys a (b*10)
                     helper (x:xs) [] a b = (fromIntegral(digitToInt x))*a + helper  xs [] (a*10) b
                     helper (x:xs) (y:ys) a b = (fromIntegral(digitToInt y))/b + (fromIntegral(digitToInt x))*a + helper  xs ys (a*10) (b*10)
                     
strToFloat2 :: String -> Float
strToFloat2 [] = 0
strToFloat2 (x:xs) | x=='-' = -1* helper xs ((length (takeWhile (/='.') xs))-1)
                  | x=='+' = helper xs ((length (takeWhile (/='.') xs))-1)
                  | otherwise = helper (x:xs) ((length (takeWhile (/='.') (x:xs)))-1)
    where
        helper (x:xs) p | p <= 0 = (fromIntegral(digitToInt x)) + helper2 xs 10
                        | otherwise = (fromIntegral(digitToInt x)) *10^p + helper xs (p-1)
            where
                helper2 [] q = 0
                helper2 (x:xs) q | x=='.' = helper2 xs q
                                 | otherwise = (fromIntegral(digitToInt x))/q+ helper2 xs (q*10)                    
                     

-- AUFGABE 5
wechselRekursiv m = length (helper m [] coins)
    where
        helper 0 ys cs = ys
        helper m ys (c:cs) | m < c = helper m ys cs
                           | otherwise = helper (m-c) (c:ys) coins 
                           
                           
wechselUnfold b = length(unfoldr (\b -> if b == 0 then Nothing else Just (f b coins,f b coins)) b)
            where
                f b (x:xs) | b>=x = b-x
                           | otherwise = f b xs
                           
coins = [200,100,50,20,10,5,2,1]


-- AUFGABE 6
abflachen :: [[a]] -> [a]
abflachen (x:xs) = foldl (++) x xs

myMin :: [Float] -> Float
myMin (x:xs) = foldl min x xs
        