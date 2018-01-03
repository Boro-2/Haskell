module Main where
import Data.Char
esito (x:xs) = x : esito[z | z<-xs, mod z x /=0]

--Zapocet: scitani mati, maximalni prvek matice, nasobeni matic, gaussova eliminace

--opakuje nekonecnokrat seznam
repeatList :: [a] -> [a]
repeatList [] = []
repeatList s = s ++ (repeatList (s))

evenList = filter ( >0) (zipWith (*) (repeatList [1, 0]) [2..]) --slozite
evenList2 = [2..] /> zipWith (*) (repeatList [1, 0]) /> filter ( >0) -- nemusim pouzivat zavorky
(/>):: a -> (a-> b) -> b
x />f = f x

stringToNumber :: String -> Integer
stringToNumber s = s /> map digitToInt 
                        /> map toInteger 
                           /> foldl (\init item -> 10 * init +item) 0 
idm :: [[Int]]--jednotkova matice nekonecna
idm = [(replicate i 0) ++ [1] ++ repeat 0 | i <-[0..]]

utm :: [[Int]]--horni trojuhlnihkova jednotkova matice nekonecna
utm = [(replicate i 0) ++ [1] ++ repeat 1 | i <-[0..]]

--udelat horni trojuhlenikovou z jakkekoliv matice
ut :: [[Int]] -> [[Int]]
ut n = zipWith (zipWith (*)) n utm --zipWith vraci operaci; zip vraci dvojici
--nemusim resit velikost utm protoze je nekonecna

--Nasobeni matic
addmm :: Num a => [[a]] -> [[a]] -> [[a]] 
addmm [] [] = [] 
addmm m1 m2 | (length (m1!!0)) == (length (m2!!0)) && (length m1)==(length m2) = [[m1!!row!!col + m2!!row!!col | col <- [0..(length (m1!!0))-1]] | row <- [0..(length m1)-1]]
            | otherwise = error "wrong sizes"

--addmm [[2,1],[4,5],[3,2]] [[1,1],[1,1],[5,5]]                              
--Pridat operace s maticemi
(***) :: Num a => [a] -> [a] -> a 
s1 *** s2 = zipWith (*) s1 s2 /> foldl (\init item -> init +item) 0

--mm [[1,1,1],[2,2,2]] [[2,1],[4,5],[3,2]]

-- vytvori seznam seznamu pro sloupec misto radku, resp vytvori matici transponovanou aby se pak dalo jednoduse pracovat s celymi sloupci matice
shiftm :: Num a => [[a]] -> [[a]]
shiftm ([]:_) = []
shiftm x = (map head x) : shiftm (map tail x) --z kazdeko radku vemu prvni polozku a z toho se vytvori seznam(sloupec matice)
                                        -- a rekurzivne volam na matici slozenou se radku, ktere uz vsak neobsahuji ten prvni prvek

--Nasobeni matic
mm :: Num a => [[a]] -> [[a]] -> [[a]] 
mm [] m2 = [] --na konci je m1 prazdna protote v kazde iteraci "ztraci" radek, tak uz na konci nic nevrati
mm m1 m2 | (==(length (m1!!0))) (length m2) = (map ((m1 !! 0)***) (shiftm m2)):mm (tail m1) m2 
                                | otherwise = error "wrong sizes"
                   -- pokud je pocet prvku v prvnim radku prvni matice (pocet sloupcu) roven poctu radku matice druhe vynasobi se, jinak hodi chybu
                   -- nasobeni probiha tak, ze vemu prvni radek m1 a delam skalarni soucin s kazdym sloupcem m2, tim vznikne prvni radek vysledne matice
                   -- k tomu se pripoji nasobeni (m1 bez 1. radku) s m2 -> vznikne druhy radek vysledne matice, atd atd atd
                
-- mm [[1,1,1],[2,2,2]] [[1,4],[2,5],[3,1]] -- pro test

gausselim:: [[Double]] -> [[Double]]
gausselim m = gausselim' m 0

--gaussova eliminace, dostane nuly pod diagonalou
gausselim':: [[Double]] -> Int -> [[Double]]
gausselim' m k | (any (>0.01) [m!!i!!k | i<-[k..((length m)-1)]] || any (< (-0.01)) [m!!i!!k | i<-[k..((length m)-1)]]) == False = error "Matice je singularni"
gausselim' m k | m!!k!!k == 0 = head [j | j<-[k..((length m)-1)], m!!j!!k /= 0] /> swapRows m k
gausselim' m k | k < nrow = gausselim' ([m!!p | p<-[0..k]] ++ [ [ m!!i!!j - (m!!i!!k/m!!k!!k)*m!!k!!j | j <- [k..ncol]] | i<-[(k+1)..nrow] ] ) (k+1)
                                |otherwise = m
                                where nrow = ((length m)-1)
                                      ncol = (length (m!!0))-1

--prohodi radeky 'a' a 'b'                                      
swapRows:: [[a]] -> Int -> Int -> [[a]]
swapRows m a b = [m!!i | i<- [0..(a-1)]] ++ [m!!b] ++ [m!!i | i<- [a+1..(b-1)]] ++ [m!!a] ++ [m!!i | i<- [b+1..((length m)-1)]]
 
       --gausselim [[1,2,3,14],[2,4,45,32],[3,6,5,36]]