module W6 where

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List
import Data.List.Split

-- Tehtävä 1: Tässä luentojen ?>-operaattori. Toteuta sitä käyttäen
-- funktio lueNimi, joka tuottaa parin (etunimi,sukunimi) kun sille
-- annetaan merkkijono "etunimi sukunimi".
--
-- Funktion tulee epäonnistua (eli palauttaa Nothing) jos
--    1. merkkijonossa ei ole välilyöntiä
--    2. jompi kumpi nimistä sisältää numeroita
--    3. nimet eivät ala isolla kirjaimella
--
-- Itse funktio lueNimet on valmiina, sinun tarvitsee vain toteutaa
-- apufunktiot pilko, tarkastaNumero ja tarkastaIsotAlkukirjaimet.

(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> _ = Nothing   -- kun on kerran epäonnistuttu, ei tehdä enää mitään
Just x  ?> f = f x       -- jos onnistuttiin, jatketaan

lueNimet :: String -> Maybe (String,String)
lueNimet s = 
  pilko s
  ?>
  tarkastaNumero
  ?>
  tarkastaIsotAlkukirjaimet

pilko :: String -> Maybe (String, String)
pilko s
  | length split > 1 = Just (head split, head $ tail split)
  | otherwise = Nothing
  where split = splitOn " " s

tarkastaNumero :: (String, String) -> Maybe (String, String)
tarkastaNumero (a,b)
  | not (containsNum a) && not (containsNum b) = Just (a, b)
  | otherwise = Nothing
  where containsNum = any (\x -> x `elem` ['0'..'9'])

tarkastaIsotAlkukirjaimet :: (String, String) -> Maybe (String, String)
tarkastaIsotAlkukirjaimet (a, b)
  | hasCapital a && hasCapital b = Just (a, b)
  | otherwise = Nothing
  where hasCapital = isUpper . head

-- Tehtävä 2: Toteuta ?>-operaattorin avulla funktio chainList, joka
-- muttaa listan Maybe-arvoja listaksi ketjuttamalla ne kaikki yhteen.
--
-- ÄLÄ käytä Mayben hahmonsovitusta, vaan operaattoria ?>! 
--
-- Esimerkkejä:
--  chainList [Just 1, Just 2, Just 3]
--    ==> Just [1,2,3]
--  chainList [Just 1, Nothing, Just 3]
--    ==> Nothing

chainList :: [Maybe a] -> Maybe [a]
chainList [] = Just []
chainList xs = chainList' xs []
  where chainList' [] list = Just list
        chainList' (x:xs) list = x ?> (\x -> chainList' xs (list ++ [x]))

-- Tehtävä 3: Toteuta chainListin ja ?>:n avulla funktio sumPos, joka
-- laskee listan summan, mutta epäonnistuu (eli palauttaa Nothing) jos
-- listalla on negatiivinen luku.
--
-- Huom! Käytä funktiota chainList ja operaattoria ?>! Älä käytä
-- Mayben hahmonsovitusta!

sumPos :: [Int] -> Maybe Int
sumPos xs = sumPos' positives 0
  where positives = map (\x -> if x >= 0 then Just x else Nothing) xs
        sumPos' [] acc = Just acc
        sumPos' (x:xs) acc = x ?> (\x -> sumPos' xs (acc + x))

-- Tehtävä 4: Toteuta Maybe-monadia käyttäen (eli siis do-notaatiota
-- tai >>=-operaattoria tai monadifunktioita) funktio myTake, joka
-- toimii kuten take, mutta
--   1. Ensimmäinen argumentti on tyyppiä Maybe Int ja toinen tyyppiä Maybe [a]
--   2. Jos kumpi tahansa argumentti on Nothing, palautetaan Nothing
--   3. Jos koitetaan ottaa listasta enemmän alkioita kuin siellä on, palautetaan Nothing
--
-- Huom! Älä käytä Mayben hahmonsovitusta.
--
-- Esimerkkejä:
--  myTake (Just 2) (Just [5,6,7])
--    ==> Just [5,6]
--  myTake Nothing (Just [5,6,7])
--    ==> Nothing
--  myTake (Just 2) Nothing
--    ==> Nothing
--  myTake (Just 4) (Just [5,6,7])
--    ==> Nothing

myTake :: Maybe Int -> Maybe [a] -> Maybe [a]
myTake mi ml = do i <- mi
                  l <- ml
                  if i > length l
                    then Nothing
                  else
                    Just (take i l)

-- Tehtävä 5: Toteuta Maybe-monadia käyttäen (eli siis do-notaatiota
-- tai >>=-operaattoria tai monadifunktioita) funktio selectSum, joka
-- saa listan lukuja ja listan indeksejä, ja tuottaa annetuissa
-- indekseissä olevien lukujen summan. Funktio epäonnistuu jos jokin
-- indeksi on liian iso tai liian pieni.
--
-- Vihje! kannattaa toteuttaa ensin funktio safeIndex :: [a] -> Int -> Maybe a
--
-- Esimerkkejä:
--  selectSum [0..10] [4,6,9]
--    Just 19
--  selectSum [0..10] [4,6,9,20]
--    Nothing

selectSum :: Num a => [a] -> [Int] -> Maybe a
selectSum xs is = safeSum' xs safeIndices 0
  where safeIndex xs i = if i >= length xs || i < 0 then Nothing else Just i
        safeIndices = map (safeIndex xs) is
        safeSum' xs [] sum  = return sum
        safeSum' xs (i:is) sum = do ind <- i
                                    safeSum' xs is (sum + xs !! ind)

-- Tehtävä 6: Alta löydät luentojen Logger-monadin toteutuksen.
-- 
-- Tehtävänäsi on laskea binomikertoimia rekursiivisesti kaavalla
--   B(n,0) = 1
--   B(0,k) = 0, kun k>0
--   B(n,k) = B(n-1,k-1) + B(n-1,k)
-- siten, että jokainen kutsu logataan. Logausten järjestyksen tulee
-- olla sama kuin suoritusjärjestyksen.
--
-- Esimerkkejä:
--   binom 0 0 ==> Logger ["B(0,0)"] 1
--   binom 0 7 ==> Logger ["B(0,7)"] 0
--   binom 1 1 ==> Logger ["B(0,0)","B(0,1)","B(1,1)"] 1
--   binom 2 2 ==> Logger ["B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)"] 1

data Logger a = Logger [String] a  deriving Show

instance Monad Logger where
  return x = Logger [] x
  Logger la a >>= f = Logger (la++lb) b
    where Logger lb b = f a
    
msg s = Logger [s] ()

binom :: Integer -> Integer -> Logger Integer
binom n 0 = do msg ("B(" ++ show n ++ ",0)")
               return 1
binom 0 k = do msg ("B(0," ++ show k ++ ")")
               return 0
binom n k = do x <- binom (n - 1) (k - 1)
               y <- binom (n - 1) k
               msg ("B(" ++ show n ++ "," ++ show k ++ ")")
               return (x + y)

-- Tehtävä 7: Kirjoita State-monadissa operaatio paivitys joka ensin
-- kertoo tilan kahdella ja sitten lisää siihen yhden. Tilatyyppi on
-- Int.
--
-- Esimerkkejä:
--  runState paivitys 3
--    ==> ((),7) 

paivitys :: State Int ()
paivitys = do i <- get
              put (2 * i + 1)

-- Tehtävä 8: Kirjoita State-monadia käyttäen operaatio
-- lengthAndCount. Operaation tulee palauttaa annetun listan pituus,
-- ja lisätä tilana olevaan lukuun annetun arvon esiintymien
-- lukumäärä.
--
-- Huom! Tee laskenta monadisesti. Älä siis käytä funktioita length
-- tai filter, vaan tee lengthAndCountista rekursiivinen.
--
-- Esimerkkejä:
--  runState (lengthAndCount True [False,True,False,True,False]) 0
--    ==> (5,2)

lengthAndCount :: Eq a => a -> [a] -> State Int Int
lengthAndCount x xs = lengthAndCount' x xs 0
  where  lengthAndCount' a [] acc = return acc
         lengthAndCount' a (x:xs) acc = do i <- get
                                           when (x == a) $ do
                                             put (i + 1)
                                           lengthAndCount' a xs (acc + 1)

-- Tehtävä 9: Tyypillä [(a,Int)] voidaan laskea alkioitten
-- esiintymiskertoja. Esimerkiksi [(True,1),(False,3)] tarkoittaa että
-- True on esiintynyt kerran ja False kolme kertaa. Toteuta
-- State-monadin operaatio count, joka rekisteröi yhden
-- esiintymiskerran annetulle alkiolle.
--
-- Esimerkkejä:
--  runState (count True) []
--    ==> ((),[(True,1)])
--  runState (count 7) []
--    ==> ((),[(7,1)])
--  runState (count 3) [(3,1),(2,3)]
--    ==> ((),[(3,2),(2,3)])
--
-- PS. Tilalistan järjestyksellä ei ole väliä. Testit järjestävät
-- listan ennen vertailua.

count :: Eq a => a -> State [(a,Int)] ()
count x = do pairs <- get
             if x `elem` vals pairs
               then put (update x pairs)
               else put (add x pairs)
  where vals pairs = map fst pairs
        add x pairs = (x, 1) : pairs
        update x [] = []
        update x ((a,c):pairs)
          | a == x = (a, c + 1) : update x pairs
          | otherwise = (a, c) : update x pairs

-- Tehtävä 10: Kirjoita State-monadia käyttäen operaatio occurrences,
-- joka muuntaa annetun listan siten, että arvon x paikalle tulee
-- numero, joka kertoo monesko arvon x esiintymä tässä paikassa oli.
--
-- Käytä tilana tyyppiä [(a,Int)], jossa voit helposti pitää yllä eri
-- arvojen esiintymiskertojen lukumääriä.
--
-- Käytä operaatiota count!
--
-- Muista funktio lookup!
--
-- Esimerkkejä:
--  runState (occurrences [True,True,True,False,False]) []
--    ==> ([1,2,3,1,2],[(True,3),(False,2)])
--  runState (occurrences [5,5,6,6,5,6,7]) []
--    ==> ([1,2,1,2,3,3,1],[(5,3),(6,3),(7,1)])


occurrences :: (Eq a) => [a] -> State [(a,Int)] [Int]
occurrences xs = occurrences' xs []
  where occurrences' [] acc = return acc
        occurrences' (x:xs) acc = do count x
                                     counts <- get
                                     occurrences' xs (acc ++ [getOcc x counts])
        getOcc x [] = 0
        getOcc x ((a,c):pairs)
          | a == x = c
          | otherwise = getOcc x pairs

-- Tehtävä 11: Toteuta funktio ifM, joka ottaa monadioperaation joka
-- palauttaa Boolin, ja jos se on True ajaa operaation opThen, ja jos
-- False operaation opFalse.
--
-- Esimerkkejä (test on määritelty alla):
--  runState (put 11 >> ifM test (return 'a') (return 'b')) 0
--    ==> ('b',11)
--  runState (put 9 >> ifM test (return 'a') (return 'b')) 0
--    ==> ('a',9)

test :: State Int Bool
test = do
  x <- get
  return (x<10)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM opBool opThen opElse = do b <- opBool
                              if b then opThen else opElse

-- Tehtävä 12: Toteuta mapM2, joka on kuin mapM, mutta listoja on
-- kaksi ja argumenttina oleva operaatio ottaa kaksi argumenttia.
--
-- Jos annetut listat ovat eripituisia, voit lopettaa prosessoinnin
-- kun toinen lista loppuu.
--
-- Esimerkkejä:
--  mapM2 (\x y -> Just (x+y)) [1,2,3] [6,7]
--    ==> Just [7,9]
--  runState (mapM2 (\x y -> if x then modify (+y) else return () ) [True,False,True] [1,2,4]) 0
--    ==> ([(),(),()],5)

mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 op xs ys = mapM2' op xs ys []
  where mapM2' op [] ys acc = return acc
        mapM2' op xs [] acc = return acc
        mapM2' op (x:xs) (y:ys) acc = do a <- op x y
                                         mapM2' op xs ys (acc ++ [a])

-- Tehtävä 13&14: Hassumaassa on kaupunkeja, jotka on nimetty
-- kokonaisluvuilla 0..n-1. Joittenkin kaupunkien välillä menee tie.
-- Tehtävänäsi on selvittää, pääseekö annetusta kaupungista toiseen.
--
-- Kaupunkien väliset yhteydet annetaan _vieruslistana_ eli
-- kaksiulotteisena listana [[Int]] joka toimii siten, että
-- kaupungista i on tiet kaikkiin i:nnen listan kaupunkeihin.
--
-- Esimerkiksi siis tieverkosto
--
-- 0--1
-- |\ |
-- | \|
-- 2--3
--
-- Ilmaistaisiin muodossa:
--  [[1,2,3]
--  ,[0,3]
--  ,[0,3]
--  ,[0,1,2]]
--
-- Alta löydät funktion routeExists, joka katsoo pääseekö kaupungista
-- i kaupunkiin j teitä pitkin liikkumalla. Funktion ydin, eli
-- tilamonadissa toimiva funktio dfs on kuitenkin toteuttamatta.
--
-- Funktion dfs on tarkoitus toteuttaa _syvyyssuuntainen_haku_ eli
-- Depth First Search. Jos et tiedä mitä tämä tarkoittaa, vilkaise
-- vaikka wikipediaa.
--
-- Yksinkertaisemmin sanottua funktio dfs lähtee liikkeelle
-- kaupungista i ja etsii kaikki kaupungit joihin i:stä pääsee teitä
-- pitkin. Tila tyyppiä [Int] pitää kirjaa siitä missä kaupungeissa on
-- jo käyty. Tämä on tärkeää sillä tieverkossa voi olla syklejä.
--
-- Esimerkkejä:
--   routeExists example1 0 2  ==> True
--   routeExists example2 0 2  ==> True
--   routeExists example2 3 5  ==> False
--   runState (dfs example2 0) []  ==> ((),[2,3,1,0])
--  Kun 1:ssä ja 2:ssa on jo käyty, dfs 0:sta alkaen ei etene niiten läpi kaupunkiin 3
--   runState (dfs example1 0) [1,2] ==> ((),[0,1,2])
--
-- Sananen testeistä: Testit testaavat ensin funktiota dfs parissa
-- yksinkertaisessa tilanteessa ja sitten koko funktiota routeExists.
-- Testit katsovat dfs:n tuottamaa tilaa, mutta eivät välitä sen
-- järkestyksestä!

-- Kolme kaupunkia, tie jokaisen parin välillä
example1 :: [[Int]]
example1 = [[1,2]
           ,[0,2]
           ,[0,1]]
           
-- Monimutkaisempi tieverkosto
example2 :: [[Int]]
example2 = [[1,2]
           ,[0,3]
           ,[0,3]
           ,[1,2]
           ,[5]
           ,[4]]
            
routeExists :: [[Int]] -> Int -> Int -> Bool
routeExists cities i j = j `elem` execState (dfs cities i) []

dfs :: [[Int]] -> Int -> State [Int] ()
dfs cities i = do visited <- get
                  if not $ i `elem` visited then put (i : visited) else put (visited)
                  mapM (dfs cities) (unvisitedNeighbors (i : visited))
                  return ()
  where neighbors = cities !! i
        unvisitedNeighbors visited =  filter (\x -> not $ x `elem` visited) neighbors

-- Tehtävä 15: Tee funktio orderedPairs, joka palauttaa kaikki parit
-- (i,j) siten, että i<j ja i on ennen j:tä listassa xs.
--
-- Käytä listamonadia!
--
-- Esimerkkejä:
--  orderedPairs [1,3,2,4]
--    ==> [(1,3),(1,2),(1,4),(3,4),(2,4)]
--
-- PS. testit eivät jälleen välitä listan järjestyksestä

orderedPairs :: [Int] -> [(Int,Int)]
orderedPairs xs = do i <- inds
                     j <- inds
                     let a = xs !! i
                         b = xs !! j
                     if (i < j && a < b) then [(a,b)] else []
  where inds = [0 .. length xs - 1]

-- Tehtävä 16: Tee funktio summat, joka laskee kaikki listan alkioita
-- summaamalla saatavat luvut.
--
-- Käytä listamonadia.
--
-- Vihje: ajattele mitä [True,False] tekee listamonadissa...
--
-- Huom! Palautetun listan järjestyksellä ei ole väliä ja siinä saa
-- olla toistuvia alkioita.
--
-- Esimerkkejä:
--   summat []
--     ==> [0]
--   summat [1]
--     ==> [1,0]
--   summat [1,2,4]
--     ==> [7,3,5,1,6,2,4,0]


summat :: [Int] -> [Int]
summat [] = [0]
summat (x:xs) = do belongs <- [True, False]
                   sumRest <- summat xs
                   if belongs then return (x + sumRest) else return sumRest

-- Tehtävä 17: Haskellin standardikirjasto määritteelee funktion
--   foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
--
-- Tämä funktio toimii kuten toissatehtävissä käsitelty foldr, mutta
-- "romauttamisfunktio" on monadinen. foldM f acc xs toimii siten,
-- että f ajetaan peräkkäin jokaiselle xs:n alkiolle siten, että f saa
-- argumenteikseen edellisen f:n kutsun palauttaman arvon ja
-- tämänhetkisen alkion.
--
-- Tehtävänäsi on toteutta apufunktiot f1 ja f2 siten, että sumBounded
-- ja sumNotTwice toimivat.

-- Funktio sumBounded laskee listan summan. Kuitenkin jos jonkin
-- listan alkupätkän summa on yli k, epäonnistuu laskenta.
--
-- Esimerkkejä:
--  sumBounded 5 [1,2,1,-2,3]
--    ==> Just 5
--  sumBounded 5 [1,2,3,1,-2]
--    ==> Nothing

sumBounded :: Int -> [Int] -> Maybe Int
sumBounded k xs = foldM (f1 k) 0 xs

f1 :: Int -> Int -> Int -> Maybe Int
f1 k acc x
  | sum > k = Nothing
  | otherwise = Just (sum)
  where sum = acc + x

-- Funktio sumNotTwice laskee listan summan, mutta jättää toistuvat
-- luvut huomiotta.
--
-- Esimerkkejä:
--  sumNotTwice [3,-2,3]
--    ==> 1
--  sumNotTwice [1,2,-2,3]
--    ==> 4

sumNotTwice :: [Int] -> Int
sumNotTwice xs = fst $ runState (foldM f2 0 xs) []

f2 :: Int -> Int -> State [Int] Int
f2 acc x = do occurred <- get
              if x `elem` occurred
                then return acc
                else do put (x : occurred)
                        return (acc + x)

-- Tehtävä 18: Tässä viime viikon tehtävistä tuttu tyyppi Result.
-- Tehtävänäsi on toteuttaa Monad Result instanssi, joka toimii
-- samantyyppisesti kuin Maybe-monadi.
--
-- Instanssin toiminnan tulisi olla seuraavanlaista:
--   1. Jos laskennan kaikki välitulokset ovat onnistuneita (eli
--   MkResult), onnistuu laskenta
--   2. Jos laskennassa tulee virhe (Failure tai NoResult), ei
--   loppulaskentaa suoriteta, vaan lopputulos on tämä virhe.
--   3. Monadifunktion fail tulee tuottaa Failure
--
-- Esimerkkejä:
--   MkResult 1 >> Failure "moi" >> MkResult 2
--     ==> Failure "moi"
--   MkResult 1 >>= (\x -> MkResult (x+1))
--     ==> MkResult 2

data Result a = MkResult a | NoResult | Failure String deriving (Show,Eq)

instance Monad Result where
  return x = MkResult x
  NoResult >>= _ = NoResult
  (Failure s) >>= _  = Failure s
  (MkResult a) >>= f = f a
  fail s = Failure s

-- Tehtävä 19&20: Tässä SL-tyyppi, joka ikäänkuin yhdistää State- ja
-- Logger-tyypit. Kirjoita instanssi Monad SL, joka kuljettaa tilaa
-- kuten monadi State, ja yhdistää laskennan vaiheitten lokiviestit
-- kuten Logger.
--
-- Tämä tehtävä on aika haastava ja tästä saakin kaksi pistettä!
--
-- Esimerkkejä:
--   runSL (putSL 2 >> msgSL "moi" >> getSL) 0
--      ==> (2,2,["moi"])
--   runSL (replicateM_ 5 (modifySL (+1) >> getSL >>= \x -> msgSL ("got "++show x))) 1
--      ==> ((),6,["got 2","got 3","got 4","got 5","got 6"])

data SL a = SL (Int -> (a,Int,[String]))

-- Aja SL-operaatio annetulla tilalla
runSL :: SL a -> Int -> (a,Int,[String])
runSL (SL f) s = f s

-- Kirjoita lokiviesti
msgSL :: String -> SL ()
msgSL msg = SL (\s -> ((),s,[msg]))

-- Hae tila
getSL :: SL Int
getSL = SL (\s -> (s,s,[]))

-- Kirjoita tila
putSL :: Int -> SL ()
putSL s' = SL (\s -> ((),s',[]))

-- Muunna tilaa
modifySL :: (Int->Int) -> SL ()
modifySL f = SL (\s -> ((),f s,[]))

instance Monad SL where
  return x = SL (\i -> (x, i, []))
