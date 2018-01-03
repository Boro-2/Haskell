--module Main where
import Data.List

--udelat koncove rekuryivni replace, sort

len :: [t] -> Integer
len [] = 0
len [x] = 1
len (_:xs) = 1 + len xs

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x1:tail@(x2:_)) = x1 < x2 && isSorted tail

lastt:: [a] -> a
lastt [] = error "Empty list"
lastt [x] = x
lastt (_:xs) = last xs


fastFilter:: (a -> Bool) -> [a] -> [a] 
-- dva vstupni parametry prvni je fce, druhy je seznam objektů a
-- posledni je navratovy typ, seznam objektu acek
fastFilter _ [] = []
fastFilter pred (x:xs) | pred x = x : filteredXs 
               | otherwise = filteredXs
            where filteredXs = fastFilter pred xs
--strazce |, pokud plati fx = tak neco
--       | jinak = neco
--   kde neco = ...

--VSE OK 


mismatch':: Int -> (a -> b -> Bool) -> [a] -> [b] -> Int 
--neco s apostrofem je private metoda
mismatch' i _ _ [] = i
mismatch' i _ [] _ = i
mismatch' i equiv (x:xs) (y:ys) | x `equiv` y = mismatch' (i+1) equiv xs ys -- x `equiv` y
                | otherwise = i

--mismatch:: (a -> b -> Bool) -> [a] -> [b] -> Int 
-- predpokladame ze je acko a bcko muzu srovnat
mismatch equiv (x:xs) (y:ys) = mismatch' 0 equiv (x:xs) (y:ys)

mySort:: Ord a => [a] -> [a]
mySort [] = []
mySort [x] = [x]
mySort (x:xs) | x < m = x : (mySort xs)
          | otherwise = m : (mySort (x:(m `delete` xs)))
        where m = minimum xs


homeSort':: Ord a => [a] -> [a] -> [a]
homeSort' [] fi@(x:xs) = homeSort' (maximum (fi):[]) (delete (maximum (fi)) (fi))
homeSort' se@(y:ys) [x] = x:se
homeSort' se@(x:xs) [] = se
homeSort' se@(y:ys) fi@(x:xs) = homeSort' ((maximum (fi)):se) (delete (maximum (fi)) (fi))

 
homeSort:: Ord a => [a] -> [a]
homeSort [] = error "empty"
homeSort [x] = [x]
homeSort s@(x:xs) = homeSort' [] s

--nest
nest:: (a->a) -> Int -> (a -> a) --funkce se 2 parametry vracejici funkci (a->a)
--nest f 0 x = x -- stare
--nest f i x = f (nest f (i-1) x) --stare
nest f 0 = id
nest f i = f . (nest f (i-1))

der::Fractional a=> (a -> a) -> (a -> a)
der f x = (f (x+h/2) - f (x-h/2))/h
            where h = 1e-2

p x = x*x -2*x
            
 -- der p (der 