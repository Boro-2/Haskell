import Data.List

-- Zapocet, rozpoznani slova automatem
-- prevest prechodovou fci na tabulku [ [A,B], [B,A] ] <-(tohle jsem si trochu upravil) na [[A,A,B],[B,B,A]]
--              -funkce ma za ukol prevest tento seznam na fci delta [[State]] -> (State -> Symbol -> State)
-- funkce vracejici rozpoznavajici jazyk DFA -> [Worda]
-- funkce vracejici automat bez nedosazitelnych stavu

(/>):: a -> (a-> b) -> b
x />f = f x

onlyTrue:: [a] -> [Bool] -> [a] -> [a]
onlyTrue [] _ c = c 
onlyTrue (a:as) (b:bs) c| b == True = (onlyTrue as bs (c++[a]))
                        | otherwise = onlyTrue as bs c

data State = A | B | C | D | E | F | G
        deriving (Show,Eq,Ord, Bounded,Enum)
data Symbol = S0 | S1 deriving (Show,Read, Eq, Ord, Bounded,Enum)

type Worda = [ Symbol ] 

data DFA = DFA [ State ] State (State -> Symbol -> State) [State]

makeDFA :: [ State ] -> State -> (State -> Symbol -> State) -> [State] -> DFA
makeDFA states initState  _ _             | initState `notElem` states = error "Invalid init state"
makeDFA states _ _ finalStates             | finalStates `isNotSubset` states = error "invalid final states"
makeDFA states initstate delta finalStates | otherwise = DFA states initstate delta finalStates

-- mam dve mnoziny, F(final) a Q(states) a kontroloju jestli je kazdy stav z F zaroven v Q. Pokud ano, vracim false, aby se druhy radek makeDFA neprovedl
isNotSubset a b = a /> map (( flip elem ) b)  /> foldl (&&) True /> not  

--delta A S0 = A
--delta A S1 = B
--delta B S0 = A
--delta B S1= C
-- ....a pokracoval bych pro vsechny stavy ... nastesti uz nemusim

--prvni pismeno je stav, kde jsem, druhe je kam se dostanu kdyz prijde S0, a treti kam se dostanu když prijde S1
--je to jednodussi nez pokazde psat vsechny pripady.
sezdelta = [[A,A,B],[B,A,C],[C,A,E],[D,A,B],[E,B,A],[F,D,A]]

-- mam nejaky seznam (viz sezdelta) a vytvorim z nej delta funkci, ktera je zapotrebi k vytvoreni automatu
deltaFromList:: [[State]] -> (State -> Symbol -> State)
deltaFromList sez state symb | symb ==S0 = head [x!!1 | x<-sez, x!!0 == state]
                             | symb ==S1 = head [x!!2 | x<-sez, x!!0 == state]


showstates:: DFA -> [State]
showstates (DFA states b c d) = states

showfinalstates:: DFA -> [State]
showfinalstates (DFA states b c final) = final

showdelta:: DFA -> [[State]]
showdelta (DFA states _ delta _) = [[x]++[(delta x y)| y<-[S0,S1]] | x<- states]

a1 = makeDFA [A,B,C,D,E,F] A (deltaFromList sezdelta) [C,D] --testovaci automat

--fce vracejici Bool, jestli je dane slovo rozpoynatelne automatem 
accept :: DFA -> Worda -> Bool 
accept (DFA _ init _ final) [] = elem init final
accept (DFA states init delta finalStates) (x:xs) = accept (DFA states (delta init x) delta finalStates) xs
--priklad: accept a1 [S0,S1,S1] 

--vrati seznam vsech slov do zadane delky, ktera jsou rozpoznatelna
lang :: DFA -> [Worda]
lang a = [ x | x <- sigma [], accept a x]

sigma:: [Worda] -> [Worda]
sigma [] = sigma [[],[S0],[S1]]
sigma slv = slv ++ sigma  ([ slv!!i++[S0] | i <- [0..((length slv)-1)], (length(slv!!i)) == (length(last slv))] 


                                         ++ [ slv!!i++[S1] | i <- [0..((length slv)-1)], (length(slv!!i)) == (length(last slv))]) 
--vraci seznam dosazitelnych stavu
reachable:: DFA -> [State]
reachable a = reachable' a [] []     
                                    
reachable':: DFA -> [State] -> [State] -> [State]
reachable' a@(DFA states inis delta final) [] [] = reachable' a [] ((delta inis S0): (delta inis S1):[])
reachable' a@(DFA states inis delta final) x z | length x < length z = reachable' a z (  nub(z ++ (map ((flip delta) S0) z) ++ (map ((flip delta) S1) z))  ) 

                                                  |otherwise = x
--vrati parcialni funkci delta, ktera neobsahuje nedosazitelne stavy
parcdelta:: Eq a => [[a]] -> [a] -> [[a]]
parcdelta _ [] = [[]]
parcdelta dlt rchbl = [ x | x <-dlt, elem (x!!0) rchbl]

-- vraci automat bez nedosazitelnych stavu
semireduced:: DFA -> DFA
semireduced a@(DFA states inis delta final) = makeDFA rchbl inis prcdl [x | x <-final , elem x rchbl]
                                            where rchbl = reachable a
                                                  prcdl = deltaFromList ( parcdelta (showdelta a) rchbl ) 
 
-- priklad: let a2 = semireduced a1
-- vrati automat a1 bez nedosazitelnych stavu. 


