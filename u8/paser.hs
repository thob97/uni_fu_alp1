--Sie können so eingeben : calc "1+1" usw...
import Data.List
import Data.Char

data Expr = Num Int | Sum Expr Expr | Sub Expr Expr | Mul Expr Expr | Divi Expr Expr deriving (Show,Eq)

--Aufgabe 4
priority '+' = 80
priority '-' = 80
priority '*' = 90
priority '/' = 90
priority '^' = 100



apply '+' a b = Sum a b             --(1) Wo wir geändert haben
apply '-' a b = Sub a b 
apply '*' a b = Mul a b
apply '/' a b = Divi a b 




exec (s,[],p) = (s,[],p)

exec (s,(')':os),p)= (s,os,p)

exec (s,('(':os),p)= exec(a,b,p)
                     where 
                        (a,b,_)=exec(s,os,0)

exec (s:ss,op:os,p) 
                   | (priority(op)>p) = exec(n,b,p)
                         where
                          (a,b,_)=exec(ss,os,priority(op))
                          n=(apply op s (head a)):(tail a)

exec (s,op:os,p)| (priority(op) <=p) =(s,op:os,p)



tokens list = tokenize (list,[],[])

tokenize ([],s,o) = ([],s,o)
tokenize ((x:xs),s,o) 
                    | (elem x ['+','-','*','/','(',')'])= tokenize (xs,s,onew)
                         where 
                            onew=(x:o)
tokenize ((x:xs),s,o)
                    | (digit x) = tokenize (rest,sn,o)
					| otherwise = tokenize (xs, s, o)
                         where
                           dig = takewhile digit (x:xs)
                           rest = dropwhile digit (x:xs)
                           sn = (Num (conv dig)):s          --(2) Wo wir geändert haben


digit x = (x>='0') && (x <= '9')

takewhile f [] = []
takewhile f (a:b) 
                  | (f a) = a:(takewhile f b)
                  | otherwise = []
dropwhile f [] = []
dropwhile f (a:b) 
                  | (f a) = (dropwhile f b)
                  | otherwise = (a:b)                  



conv [] = 0
conv (x:xs)  = ((ord x)-(ord '0'))+ (conv xs)*10

calc list = result 
            where
              (_,b,c)=tokens (reverse list)
              (result:_,_,_) = exec (b,c,0)



                         

