xor :: Bool -> Bool -> Bool --Übungsgruppe：Qianli Wang,Thore Brehmer
xor True True = False       --Aufgabe 1
xor True False = True		--"Antivalenz"
xor False True = True
xor False False = False

prim :: Int -> Bool		--Die Funktion zum Pimzahltest
prim z = prim' z 2
	where
		prim' :: Int -> Int -> Bool
		prim' z v
			|(z-1) < v = True
			|mod z v == 0 = False
			|otherwise = prim' z (v+1)
			
summe_aller_Primzahlen :: Int -> Int -> Int  --Aufgabe 4
summe_aller_Primzahlen k n
    |k<=1 = summe_aller_Primzahlen (k+1) n				--1 ist keine Primzahl, k<1 es gibt keine negativen Primzahl
	|k>n = 0											--Abbruch falls bei n angekommen/ k>n
	|prim k==True = k+(summe_aller_Primzahlen (k+1) n)	--Wenn Primzahl dann addieren
	|otherwise = summe_aller_Primzahlen (k+1) n			--Sonst nur Rekursion
		
