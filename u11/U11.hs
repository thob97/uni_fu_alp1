--Aufgabe 1
notp = pr notp (const 1) (p 2)
andp = pr andp (const 0) (p 3)
orp  = pr orp (p 1) (const 1)
--Aufgabe 2
oddp xs = pr oddp (p 1) (p 1)  ((predd xs):xs)
evenp xs = notp  (arg1 (oddp xs))
--Aufgabe 3
cutoddp xs = suc (arg1(pr cutoddp (const (0)) (p 1) (arg1(predd xs))))        --cut für odd Zahlen
cutevenp xs = suc (arg1(pr cutevenp (const (-1)) (p 1) (arg1(predd xs))))     --cut für even Zahlen
cutp xs = p (suc(arg1(oddp xs))) ((cutevenp xs): (arg1(cutoddp xs)))          --cut mit Fallunterscheidung
--Aufgabe 4
geq xs = geq' ( (p 1 xs) : (arg1(suc (arg1(p 2 xs)))) )                       --geq (x:y+1:[])
geq' xs = pr geq' (testzero) (p 1) ( (p 1 xs) : (arg1(predd (arg1(p 2 xs))))) --geq (x-1:y-1:[])
testzero xs = pr testzero (const 1) (const 0) xs                              --test auf Zero

lee xs = geq' ( (p 2 xs) : (arg1(suc (arg1(p 1 xs)))) )                       --lee (x+1:y:[])= geq (y-1:x-1:[])
eq xs = andp ((lee xs): (arg1(geq xs)))

--Aufgabe 5
fac xs = pr fac (const 1) (compose (mul) ([(p 1), (compose suc [p 2])])) xs   --2tes compose damit anstatt mul 1 (x-1), mul 1 x gerechnet wird

--Aufgabe 6
pow xs = pow' ((p 2 xs) : (arg1(p 1 xs)))                                     --pow (x:y) = pow' (y:x)
pow' xs = pr pow' (const 1) (compose (mul) ([p 1, (compose (p 1) [p 3])])) xs 

--Aufgabe 7
fib xs = fib' (arg1(predd xs))
fib' xs = pr fib' (const 1) (compose add [(p 1),(const (fib' (arg1(predd(arg1(predd xs))) )))] ) xs
--fib' xs = pr fib' (const 1) (compose add [(p 1),(const (fib' (arg1(sub([(p 1 xs),(2)]) ))))] ) xs

{-Aus Vorlesung
All functions produce a natural number as a result. Naturals start from 0.
Tuples are represented by lists. Functions have tuples (lists) as arguments.
------------------------------------------}

-- Zero function
clr _ = 0

--The constants are produced using Haskellâ€™s â€žconstâ€œ function.
-- const n _ = n

--Successor function
suc [x] = x+1

--Projection functions. Tuples are represented by lists
p::Int->[Int]->Int
p 1 (x:xs) = x
p n (x:xs) = p (n-1) xs

-- Operations------------------------

-- Composition
compose g fs xs = g [f xs|f<-fs]


-- primitive recursion
pr::([Int]->Int)->([Int]->Int)->([Int]->Int)->[Int]->Int
pr phi psi chi (  0  :xs) = psi xs
pr phi psi chi (y:xs) = chi ((phi ((y-1):xs)):(y-1):xs)


-- Convert an argument to a list
arg1 x = [x]

------------------------ some primitive recursive functions

--predecessor of a number (predecesor of 0 is 0)
predd::[Int]->Int
predd  = pr predd (const 0) (p 2)

--Addition of two numbers
add::[Int]->Int
add xs  = pr add (p 1) (compose suc [(p 1)]) xs

--Test of equality to zero (â€žtrueâ€œ is 1, â€žfalseâ€œ is 0)
eq0  = pr eq0 (const 1) (const 0)

-- Multiplication of two numbers
mul::[Int]->Int
mul  = pr mul (clr) (compose add [(p 1),(p 3)])

-- Subtraction (m-n, â€žsubâ€œ inverts the order of the arguments and calls â€žsubâ€™â€œ, that computes -n+m )
sub  = compose sub' [(p 2),(p 1)]
sub'  = pr sub' (p 1) (compose predd [(p 1)])