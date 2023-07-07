data Nat = Zero | S Nat deriving Show

toInt :: Nat -> Int
toInt Zero = 0
toInt (S a) = 1 + (toInt a)  -- S für Nachfolger

fromInt :: Int -> Nat
fromInt 0 = Zero
fromInt n = S (fromInt (n-1))

isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False

add:: Nat -> Nat -> Nat
add a Zero = a
add Zero b = b
add (S a) b = (add a (S b))

mult :: Nat -> Nat -> Nat
mult a Zero = Zero
mult Zero b = Zero
mult (S Zero) b = b
mult a (S Zero) = a
mult a b = helper a a b
    where
        helper a b Zero = b
        helper a b (S Zero) = b
        helper a b (S n) = helper a (add a b) n
        
(.|.) :: Int -> Int -> Int
(.|.) a 0 = a
(.|.) 0 b = b
(.|.) a b | a==b = a
        | (a>b) = (.|.) (a-b) b
        | (a<b) = (.|.) a (b-a)

abflachen :: [a] -> [a]
abflachen [] = []
abflachen (x:xs) = x: abflachen xs

ohneLetztes :: [a] -> [a]
ohneLetztes [] = []
ohneLetztes (x:[]) = []
ohneLetztes (x:xs)= [x]++ ohneLetztes xs

isPalindrom :: [Integer] -> Bool
isPalindrom [] = True
isPalindrom xs = helper xs (drehen xs)
    where
        helper (x:xs) (y:ys) | xs == [] = True
                             | x == y = helper xs ys
                             | otherwise = False
 
drehen :: [a] -> [a]    -- Hilfsfunktion für isPalindrom
drehen [] = []
drehen (x:xs) = drehen xs ++[x]

tupelize :: [a] -> [b] -> [(a,b)]
tupelize [] _ = []
tupelize _ [] = []
tupelize (x:xs) (y:ys) = [(x,y)]++ tupelize xs ys

detupelize :: [(a,b)] -> ([a],[b])
detupelize (xs)= ([(fst x)| x<-xs],[(snd x)|x<-xs])

test xs ys = [(x,y)| x<-xs, y<-ys, y==0]