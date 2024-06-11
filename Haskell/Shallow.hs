{-# LANGUAGE TupleSections #-}
module Shallow where

import Data.List hiding (union)
import qualified Data.Set as S
import Debug.Trace


{-
    Punct bidimensional, reprezentat ca pereche de coordonate reale (x, y).
    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Point = (Float, Float)

{-
    Tip de funcție care primește un punct, și este parametrizat în raport
    cu tipul rezultatului.
-}
type Pointed a = Point -> a

{-
    Regiune bidimensională, reprezentată ca o funcție caracteristică
    (Point -> Bool). Pentru un punct care aparține regiunii, se întoarce True;
    altfel, False.
-}
type Region = Pointed Bool

{-
    Transformare bidimensională, reprezentată ca o funcție peste puncte.
-}
type Transformation = Point -> Point

{-
    *** TODO ***
    Implementați funcția inside, care verifică dacă un punct aparține unei
    regiuni (ea însăși reprezentată ca o funcție caracteristică).

    Constrângeri: funcția trebuie implementată point-free.

    Hint: implementați mai întâi funcția cu explicitarea parametrului formal
    (point-wise), și de-abia apoi transformați-o în stil point-free.

    Exemple:

    > inside (0, 0) (== (0, 0))
    True

    > inside (1, 1) (== (0, 0))
    False
-}

inside :: Point -> Region -> Bool
inside = flip id -- functia idenitate

{-
    *** TODO ***

    Implementați funcția fromPoints, care construiește o regiune pe baza unei
    liste de puncte.

    Constrângeri: funcția trebuie implementată point-free, fără recursivitate
    explicită.

    Exemple:

    > fromPoints [(0, 0), (1, 1)] (0, 0)
    True

    > inside (0, 0) $ fromPoints [(0, 0), (1, 1)]  -- echivalentă cu anterioara
    True

    > fromPoints [(0, 0), (1, 1)] (0, 1)
    False

    > inside (0, 1) $ fromPoints [(0, 0), (1, 1)]  -- echivalentă cu anterioara
    False
-}

fromPoints :: [Point] -> Region
fromPoints = flip elemc -- verificam daca un punct se afla in lista de puncte

{-
    *** TODO ***

    Implementați funcția rectangle, care generează o regiune aferentă
    unui dreptunghi, cu lățime și înălțime date, simetric față de originea
    (0, 0). De exemplu, un dreptunghi cu lățimea 2 și înălțimea 2 va avea
    punctul din stânga-sus (-1, 1), iar din dreapta-jos, (1, -1).

    Exemple:

    > rectangle 2 2 (0, 0)
    True

    > rectangle 2 2 (-1, 1)
    True

    > rectangle 2 2 (1, -1)
    True

    > rectangle 2 2 (2, 2)  
    False
-}

rectangle :: Float -> Float -> Region
rectangle width height = \(x, y) -> abs x <= width / 2 && abs y <= height / 2

{-
    *** TODO ***

    Implementați funcția circle, care generează o regiune aferentă unui cerc,
    cu rază dată și centrul în originea (0, 0).

    Exemple:

    > circle 1 (0, 0)
    True

    > circle 1 (1, 0)
    True
    
    > circle 1 (0, 1)
    True
    
    > circle 1 (1, 1)
    False
-}

circle :: Float -> Region
circle radius = \(x, y) -> x ^ 2 + y ^ 2 <= radius ^ 2

{-
    *** TODO ***

    Implementați funcția plot, care generează diagrama unei regiuni,
    pe o suprafață de desenare de dimensiuni fixate. Punctul (0, 0)
    se află în centrul suprafeței de desenare, iar lățimea și înălțimea
    unui cadran (dintre cele 4) sunt primite ca parametri. De exemplu, dacă
    lățimea este 2 și înălțimea este 1, punctul din stânga-sus al suprafeței
    este (-2, 1), iar cel din dreapta-jos, (2, -1). Pentru fiecare punct
    cu coordonate întregi de pe suprafața de desenare, se introduce caracterul
    '*', dacă punctul aparține regiunii de desenat, sau '.', altfel. Funcția
    se utilizează în conjuncție cu funcția printPlot, definită mai jos
    în schelet, pentru o mai bună vizualizare.

    Constrângeri: funcția trebuie implementată cu list comprehensions,
    fără recursivitate explicită.

    Hints:
    * fromIntegral pentru conversia de la Int la Float.
    * intercalate pentru alipirea mai multor liste folosind un element
      de legătură.

    Exemple:

    > printPlot 2 1 $ fromPoints [(0, 0), (1, 1)]
    ...*.
    ..*..
    .....

    > printPlot 2 2 $ rectangle 2 2
    .....
    .***.
    .***.
    .***.
    .....

    Deși dimensiunile dreptunghiului sunt 2 și 2, apariția a câte 3 caractere
    '*' pe orizontală și pe verticală poate fi înțeleasă dacă vă gândiți
    la coordonatele vizate, -1, 0 și 1, în toate combinațiile (x, y).

    > printPlot 2 2 $ circle 2     
    ..*..
    .***.
    *****
    .***.
    ..*..
-}

plot :: Int -> Int -> Region -> String
plot width height region = 
    intercalate "\n" [[if region (fromIntegral x, fromIntegral y) then '*' else '.' | x <- [-width..width]] | y <- [height, height-1 .. -height]]
{-
    Generez un rand orizontal de la height la -height
    Iterez peste x de la -width la width si verific daca (x,y ) apartine regiunii si adaug *

-}

{-
    Utilizați această funcție pentru vizualizarea diagramelor,
    după ce implementați funcția plot.
-}

printPlot :: Int -> Int -> Region -> IO ()
printPlot width height region = putStrLn $ plot width height region

{-
    *** TODO ***

    Implementați funcțiile promoteUnary și promoteBinary, care primesc
    o funcție unară (a -> b), respectiv binară (a -> b -> c), și o promovează
    pentru a opera pe rezultatul(-ele) unor funcții (Point -> a) etc.

    Constrângeri: funcția promoteUnary trebuie implementată point-free.

    Hint: dacă expandăm referirile la Pointed din tipul funcției promoteUnary,
    obținem (a -> b) -> (Point -> a) -> (Point -> b). Practic, trebuie
    construită o funcție cu tipul (Point -> b), pornind de la o funcție cu tipul
    (Point -> a) și aplicând apoi funcția cu tipul (a -> b) pe rezultatul ei.
    Extindeți apoi ideea pentru promoteBinary.

    Exemple:

    > promoteUnary (+ 1) (\(x, _) -> x) (3, 2)
    4.0

    > promoteBinary (+) (\(x, _) -> x) (\(_, y) -> y) (3, 2)
    5.0

    "promovam" (a _-> b) ai sa poata fi aplicata pe rezultatul lui (Point -> a) si sa obtinem (Point -> b)
-}

promoteUnary :: (a -> b) -> Pointed a -> Pointed b
promoteUnary f g = f . g -- compozitia functiilor

promoteBinary :: (a -> b -> c) -> Pointed a -> Pointed b -> Pointed c
promoteBinary f g h = \x -> f (g x) (h x) -- aplicam g pe x pt a obtine valoare tip a
                                          -- aplicam h pe x pt a obtine valoare tip b
                                          -- aplicam f pe cele 2 valori obtinute

{-
    *** TODO ***

    Implementați funcțiile complement, union și intersection, care determină
    complementul, reuniunea, respectiv intersecția a două regiuni.

    Constrângeri: funcțiile trebuie implementate point-free, utilizând
    promoteUnary sau promoteBinary, după caz.

    Exemple:

    > printPlot 2 2 $ complement $ circle 2
    **.**
    *...*
    .....
    *...*
    **.**

    > printPlot 2 2 $ union (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)])
    *....
    ..*..
    .***.
    ..*..
    ....*

    > printPlot 2 2 $ intersection (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)])
    .....
    .....
    ..*..
    .....
    .....
-}
complement :: Region -> Region
complement = promoteUnary not -- complementul unei reginui date, negatia functiei caracteristice

union :: Region -> Region -> Region
union = promoteBinary (||) -- det. reuniunea a 2 regiuni date

intersection :: Region -> Region -> Region
intersection = promoteBinary (&&) -- det. intersectia a 2 regiuni date, regiune care include pct ambelor regiuni

{-
    *** TODO ***

    Implementați funcția translation, care generează o translație
    cu deplasamente primite ca parametri. Deși contraintuitiv, deplasamentele
    trebuie scăzute, nu adunate, din coordonatele punctului transformat.
    De exemplu, dacă punctul (0, 0) aparține unei regiuni de interes, atunci
    punctul (1, 2) va trebui să aparțină regiunii în urma translației
    cu deplasamentele 1 și 2. Din moment ce funcția caracteristică a regiunii
    întoarce True pentru (0, 0), nu pentru (1, 2), cele două deplasamente
    trebuie scăzute.

    Exemple:

    > translation 1 2 (1, 2)
    (0.0,0.0)
-}

translation :: Float -> Float -> Transformation
translation tx ty = \(x, y) -> (x - tx, y - ty)

{-
    *** TODO ***

    Implementați funcția scaling, care generează o scalare cu un factor primit
    ca parametru. Similar cu observația de la funcția translate, factorul
    contribuie prin împărțire, nu prin înmulțire.

    Exemple:

    > scaling 2 (2, 2)
    (1.0,1.0)
-}

scaling :: Float -> Transformation
scaling factor = \(x, y) -> (x / factor, y / factor)

{-
    *** TODO ***

    Implementați funcția applyTransformation, care aplică o transformare asupra
    unei regiuni.

    Constrângeri: funcția trebuie implementată point-free.

    Exemple:

    > printPlot 2 2 $ applyTransformation (translation 1 0) (circle 2)
    ...*.
    ..***
    .****
    ..***
    ...*.

    > printPlot 2 2 $ applyTransformation (scaling 0.5) (circle 2)    
    .....
    ..*..
    .***.
    ..*..
    .....
-}

applyTransformation :: Transformation -> Region -> Region
applyTransformation t r = r . t

{-
    *** TODO ***

    Implementați funcția combineTransformations, care combină transformările
    dintr-o listă într-o singură transformare. Ordinea de aplicare
    a transformărilor este dată de ordinea lor în listă.

    Constrângeri: funcția trebuie implementată point-free, fără recursivitate
    explicită.

    Exemple:

    > printPlot 2 2 $ applyTransformation
        (combineTransformations [translation 1 0, scaling 0.5]) (circle 2)
    .....
    ...*.
    ..***
    ...*.
    .....

    Echivalent cu:

    > printPlot 2 2 $ applyTransformation (translation 1 0) $
        applyTransformation (scaling 0.5) (circle 2)
-}

combineTransformations :: [Transformation] -> Transformation
combineTransformations = foldl (flip (.)) id -- reduc o lista la o singura valoare

{-
    *** TODO ***

    Funcția circles de mai jos generează o regiune formată din n cercuri de rază
    2, translatate succesiv cu 6 unități pe orizontală.

    Explicați la prezentare utilitatea evaluării leneșe pentru determinarea
    eficientă a apartenenței unui punct la regiunea construită prin reuniune.

    Hint: utilizați trace (vezi laboratorul 7) în funcția circle pentru afișarea
    punctului primit ca parametru și evaluați expresiile de mai jos:
    > inside (0, 0) $ circles 3
    > inside (6, 0) $ circles 3
    > inside (12, 0) $ circles 3
    > inside (18, 0) $ circles 3

    Exemple:

    > printPlot 15 3 $ circles 3
    ...............................
    ...............*.....*.....*...
    ..............***...***...***..
    .............*****.*****.*****.
    ..............***...***...***..
    ...............*.....*.....*...
    ...............................

    Răspuns: Fără evaluare leneșă, pentru a determina dacă un punct se află în
    regiunea formată prin reuniunea a n cercuri,
    ar trebui să verificăm apartenența punctului la fiecare cerc în parte,
    ceea ce ar avea complexitate O(n).
-}
circles :: Int -> Region
circles n
    | n <= 0    = const False
    | otherwise = union (circle 2)
                        (applyTransformation (translation 6 0)
                                             (circles (n - 1)))

{-
    *** TODO ***

    Explicați la prezentare cum se comporta reuniunea infinita de mai jos
    când se verifica apartenenta unui punct care NU aparține regiunii.

    Răspuns: Din moment ce punctul nu se află în niciun cerc, evaluarea va
    continua la infinit, fără a găsi vreodată un cerc în care punctul se află.
    Acest lucru se intampla din cauza faptului că lista de cercuri este infinita, deci
    evaluarea nu se va opri niciodata.

    Acest comportament este un exemplu de "non-terminare", un fenomen care
    poate apărea în limbajele de programare cu evaluare leneșă atunci când o
    expresie nu poate fi evaluată la o valoare finală. În acest caz, programul va
    continua să ruleze la infinit, fără a produce un rezultat.
-}
infiniteCircles :: Region
infiniteCircles = union (circle 2)
                        (applyTransformation (translation 6 0)
                                             infiniteCircles)

{-
    *** TODO BONUS ***

    Implementați funcția bfs, care realizează o căutare în lățime într-un spațiu
    oarecare de stări de tipul a, pornind de la o stare inițială start și de la
    o funcție expand, care determină pentru o stare curentă lista stărilor vecin.
    Funcția întoarce o listă, eventual infinită, de perechi de forma
    (stare, distanță), unde stările sunt ordonate conform parcurgerii în lățime,
    iar distanța reprezintă numărul de expandări realizate pentru a obține
    starea respectivă.

    Atenție! Pot exista multiple căi către aceeași stare, astfel fiind necesară
    reținerea stărilor deja vizitate utilizând o mulțime (Set, vezi modulul
    Data.Set). Observați la începutul acestui fișier linia "import qualified
    Data.Set as S". Acest lucru înseamnă că toate tipurile și funcțiile din acel
    modul trebuie prefixate cu "S."; de exemplu: S.Set, S.insert etc.

    Hint: dacă ar exista o cale unică de la starea inițială la orice altă stare,
    nefiind necesară reținerea stărilor deja vizitate, și nici nu s-ar solicita
    calculul distanțelor, funcția ar putea fi implementată prin:

    bfs :: a -> (a -> [a]) -> [a]
    bfs start expand = result
      where
        result = start : concat (map expand result)

    map operează independent pe stări și este insuficient de expresiv pentru
    a permite purtarea mulțimii de stări vizitate de la o stare la alta. Pentru
    acest lucru, ar fi necesar foldl. Funcționala predefinită mapAccumL
    realizează exact această combinație, map + foldl. O puteți utiliza pentru
    a extinde implementarea de mai sus.
-}

bfs :: (Ord a) => a -> (a -> [a]) -> [(a, Int)]
bfs start expand = bfs' [(start, 0)] S.empty
  where
    bfs' [] _ = []
    bfs' ((current, dist):queue) visited
        -- verificam daca nodul curent este vizitat
      | current `S.member` visited = bfs' queue visited
        -- daca e vizitat trecem la urmatorul nod din lista
        -- daca nu e vizitat adaugam nodul si distanta la rezultat si continuam cautarea
      | otherwise = (current, dist) : bfs' (queue ++ map (,dist+1) (expand current)) (S.insert current visited)

{-
    *** TODO BONUS ***

    Implementați funcția regionAvoidingBfs, care determină distanța minimă
    de la orice punct la un nod de start, obținută prin deplasări către nord,
    est, sud sau vest, și ocolind regiunea primită ca parametru.

    Constrângeri: utilizați funcția bfs.

    Exemple:

    > lookup (3, 0) $ regionAvoidingBfs (-3, 0) $ circles 3
    Just 12

    Explicație: distanța de la punctul (-3, 0) la punctul (3, 0) este 12,
    și este descrisă mai jos, prin distanțele către punctele intermediare.
    Au fost folosite și cifre hexazecimale, pentru încadrarea într-un singur
    caracter. Distanța 0 corespunde punctului (-3, 0), iar distanța C (12),
    punctului (3, 0).

    ...................567...................
    ..................34*89...*.....*........
    .................12***AB.***...***.......
    .................0*****C*****.*****......
    ...................***...***...***.......
    ....................*.....*.....*........
    .........................................
-}

-- functie ajutatoare care genereaza vecinii unui punct
neighbors :: Point -> [Point]
neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

regionAvoidingBfs :: Point -> Region -> [(Point, Int)]
regionAvoidingBfs start region = bfs [(start, 0)] S.empty
  where
    bfs [] _ = []
    bfs ((p, d):ps) visited
        -- daca curr apartine regiunii il ignoram
      | region p = bfs ps visited
        -- daca nodul este vizitat trecem la urmatorul nod
      | p `S.member` visited = bfs ps visited
        -- daca nu e vizitat si nu e in regiune adaugam nodul si distanta la rezultat si continuam cautarea
      | otherwise = (p, d) : bfs (ps ++ [(n, d+1) | n <- neighbors p, not (region n)]) (S.insert p visited)

