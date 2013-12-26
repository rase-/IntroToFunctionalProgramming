module W7 where

import Data.List
import Control.Monad.State

-- Tehtävä 1: Toteuta funktio pyramidi, joka tuotaa merkkijonon, joka
-- on tähän tyyliin piirretty pyramidi:
--      *
--     ***
--    *****
--   *******
--  *********
-- ***********
--
-- Funktio ottaa argumenttina pyramidin korkeuden riveinä.
--
-- Esimerkkejä:
--   pyramidi 1 ==> "*\n"
--   pyramidi 2 ==> " *\n***\n"
--   pyramidi 3 ==> "  *\n ***\n*****\n"
--
-- PS. pyramidin ulkonäköä on helppo kokeilla ghci:ssä näin: putStr (pyramidi 5)

pyramidi :: Int -> String
pyramidi n = unlines . addSpacing $ map layer [1..n]
  where layer 1 = "*"
        layer n = concat $ replicate (starCount n) "*"
        starCount n = (2 * n) - 1
        addSpacing [] = []
        addSpacing (l:layers) = ((concat $ replicate (length layers) " ") ++ l) : addSpacing layers

-- Tehtävä 2: Toteuta funktio jokaToinen, joka ottaa listan ja
-- palauttaa kaikki listan ensimmäisen, kolmannen, viidennen, jne.
-- alkion.
--
-- HUOM!!! Älä käytä listafunktioita (head, tail, map, reverse, !!,
-- jne), vaan pelkästään hahmonsovitusta!
--
-- Esimerkkejä:
--  jokaToinen [1,2,3,4,5]
--    ==> [1,3,5]  
--  jokaToinen [1,2,3,4,5,6]
--    ==> [1,3,5]
--  jokaToinen []
--    ==> []

jokaToinen :: [a] -> [a]
jokaToinen [] = []
jokaToinen [x] = [x]
jokaToinen (x:y:xs) = x : jokaToinen xs

-- Tehtävä 3: Toteuta funktio wrap, joka ottaa listan ja palauttaa
-- parin (get,query). get ja query ovat funktioita siten, että
--   * get i palauttaa alkuperäisen listan i:nnen alkion
--   * query x palauttaa True jos x on alkuperäisessä listassa ja False muuten
--
-- Esimerkkejä:
--  let (get,query) = wrap [5,6,7] in (get 0, query 6, get 2, query 2)
--    ==> (5,True,7,False)

wrap :: Eq a => [a] -> (Int -> a, a -> Bool)
wrap xs = (get, query)
  where get i = xs !! i
        query x = x `elem` xs

-- Tehtävä 4: Toteuta funktio nousevat, joka pilkkoo lukulistan
-- (aidosti) nouseviin pätkiin.
--
-- Saat käyttää kaikkia standardikirjaston listafunktioita.
--
-- Esimerkkejä:
--  nousevat [1,2,3] ==> [[1,2,3]]
--  nousevat [1,1] ==> [[1],[1]]
--  nousevat [4,7,9,3,6,1,2,2,5,8,0]
--    ==> [[4,7,9],[3,6],[1,2],[2,5,8],[0]]

nousevat :: [Int] -> [[Int]]
nousevat [] = []
nousevat xs = inc : nousevat rest
  where (inc, rest) = splitSeq ([head xs], tail xs) (head xs)
        splitSeq (a,[]) c = (a,[])
        splitSeq (a,x:xs) c = if c < x
                                then  splitSeq (a ++ [x], xs) x
                                else (a, x:xs)

-- Tehtävä 5: Määrittele kurssilaista esittävä tietotyyppi
-- Student, jolla on kolme kenttää: nimi (String),
-- opiskelijanumero (String) ja pistemäärä (Int).
--
-- Määrittele myös funktiot:
--   * newStudent nimi opnro -- palauttaa Student-arvon jolla on annettu nimi ja opnro, ja 0 pistettä
--   * getName s -- palauttaa s:n nimen
--   * getNumber s -- palauttaa s:n opnron
--   * getPoints s -- palauttaa s:n pistemäärän
--   * addPoints i s -- lisää s:lle i pistettä. Jos i ei ole positiivinen, säilyy pistemäärä ennallaan
--
-- Esimerkkejä:
--  getName $ newStudent "pekka" "0123"
--    ==> "pekka"
--  getNumber $ newStudent "pekka" "0123"
--    ==> "0123"
--  getPoints $ newStudent "pekka" "0123"
--    ==> 0
--  getPoints $ addPoints 100 $ addPoints 100 $ newStudent "pekka" "0123"
--    ==> 200
--  getPoints $ addPoints (-1000) $ newStudent "x" "0"
--    ==> 0

data Student = Student String String Int

newStudent :: String -> String -> Student
newStudent nam num = Student nam num 0

getName :: Student -> String
getName (Student nam num op) = nam

getNumber :: Student -> String
getNumber (Student nam num op) = num

getPoints :: Student -> Int
getPoints (Student nam num op) = op

addPoints :: Int -> Student -> Student
addPoints x (Student nam num op)
  | x > 0 = Student nam num (op + x)
  | otherwise = Student nam num op

-- Tehtävä 6: Määrittele tyyppi Tree23, joka esittää puuta jossa
-- jokaisella sisäsolmulla (eli ei-lehti-solmulla) on joko 2 tai 3
-- lasta.
--
-- Sisä- ja lehtisolmujen ei tarvitse sisältää mitään muita kenttiä.
--
-- Määrittele funktiot treeHeight ja treeSize jotka laskevat Tree23:n
-- korkeuden ja koon (eli sisäsolmujen määrän).
--
-- Testaamisen mahdollistamiseksi määrittele myös funktiot node2 ja
-- node3 jotka luovat kaksi- ja kolmelapsisen solmun sekä funktio
-- (taino, vakio) leaf joka luo lehden.
--
-- PS! Muista jättää deriving Show -rivi paikalleen että testit voivat
-- tulostaa asioita.

data Tree23 = Leaf | Tree2 Tree23 Tree23 | Tree3 Tree23 Tree23 Tree23
  deriving Show

leaf :: Tree23
leaf = Leaf

node2 :: Tree23 -> Tree23 -> Tree23
node2 a b = Tree2 a b

node3 :: Tree23 -> Tree23 -> Tree23 -> Tree23
node3 a b c = Tree3 a b c

treeHeight :: Tree23 -> Int
treeHeight Leaf = 0
treeHeight (Tree3 a b c) = 1 + maximum [treeHeight a, treeHeight b, treeHeight c]
treeHeight (Tree2 a b) = 1 + maximum [treeHeight a, treeHeight b]

treeSize :: Tree23 -> Int
treeSize Leaf = 0
treeSize (Tree3 a b c) = 1 + treeSize a + treeSize b + treeSize c
treeSize (Tree2 a b) = 1 + treeSize a + treeSize b

-- Tehtävä 7: Määrittele tyyppi MyString, ja sille Eq ja Ord
-- -instanssit.
--
-- Testaamista varten määrittele myös funktiot fromString ja toString.
--
-- MyStringin Ord-instanssin tulee järjestää merkkijonot
-- _leksikografiseen_ järjestykseen. Tämä tarkoittaa sitä että
-- lyhyempi merkkijono tulee aina ennen pidempää merkkijonoa, ja
-- samanpituiset merkkijonot ovat aakkosjärjestyksessä.
-- 
-- Huom! Saat valita tyypin MyString toteutuksen täysin vapaasti,
-- testit käyttävät vain Eq- ja Ord-instansseja ja fromString ja
-- toString funktioita.
--
-- Esimerkkejä:
--
-- fromString "xyz" == fromString "xyz"          ==> True
-- fromString "xyz" == fromString "xyw"          ==> False
--
-- compare (fromString "abc") (fromString "ab")  ==> GT
-- compare (fromString "abc") (fromString "abd") ==> LT

data MyString = MyString String

fromString :: String -> MyString
fromString s = MyString s

toString :: MyString -> String
toString (MyString s) = s

instance Eq MyString where
  (MyString a) == (MyString b) = a == b
  
instance Ord MyString where
  compare (MyString a) (MyString b)
    | length a < length b = LT
    | length a > length b = GT
    | otherwise = compare a b

-- Tehtävä 8: Alla tyyppi Expr, joka kuvaa yhteen- ja jakolaskuista
-- koostuvia laskutoimituksia. Esimerkiksi (1+2)/3+4 olisi
--   Plus (Div (Plus (Constant 1) (Constant 2)) (Constant 3)) (Constant 4)
--
-- Toteuta funktio safeEval :: Expr -> Maybe Int, joka laskee annetun
-- lausekkeen arvon. safeEval palauttaa Nothing jos lausekketta
-- laskiessa tapahtuu nollalla jakaminen.
--
-- Vihje: Maybe-monadi
--
-- Esimerkkejä:
--   safeEval (Plus (Constant 1) (Constant 1))
--     ==> Just 2
--   safeEval (Div (Constant 6) (Constant 2))
--     ==> Just 3
--   safeEval (Div (Constant 6) (Constant 0))
--     ==> Nothing
--   safeEval (Plus (Constant 1) (Div (Constant 8) (Plus (Constant 2) (Constant (-2)))))
--     ==> Nothing

data Expr = Constant Int | Plus Expr Expr | Div Expr Expr

safeEval :: Expr -> Maybe Int
safeEval (Constant i) = Just i
safeEval (Plus a b) = do i <- safeEval a
                         j <- safeEval b
                         return $ i + j
safeEval (Div a b) = do i <- safeEval a
                        j <- safeEval b
                        if j == 0 then Nothing else return $ div i j

-- Tehtävä 9: Toteuta operaatio test, joka saa listan monadisia
-- testejä ja arvon. test palauttaa True jos kaikki testit palauttavat
-- arvolle True, ja False muuten.
--
-- Testejä tulee ajaa vain ensimmäiseen Falsen palauttavaan testiin asti!
--
-- Esimerkkejä:
--
-- Simppeleitä maybe-testejä:
--  test [test1 2, test1 3, test1 5] 7
--   ==> Just True
--  test [test1 2, test1 3, test1 5] 4
--   ==> Just False
--  test [test1 2, test1 3, failTest] 4
--   ==> Nothing
--  test [test1 2, test1 3, failTest] 1
--   ==> Just False
--
-- Pidetään kirjaa ajetuista testeistä State-monadilla:
--  runState (test [test2 4, test2 8, test2 10] 11) []
--   ==> (True,[10,8,4])
--  runState (test [test2 4, test2 8, test2 10] 5) []
--   ==> (False,[8,4])
--  runState (test [test2 4, test2 8, test2 10] 0) []
--   ==> (False,[4])

test1 :: Int -> Int -> Maybe Bool
test1 k x = Just (x>k)

failTest :: Int -> Maybe Bool
failTest x = Nothing

test2 :: Int -> Int -> State [Int] Bool
test2 k x = do modify (k:)
               return (x>k)

test :: Monad m => [a -> m Bool] -> a -> m Bool
test [] a = return True
test (t:ts) a = do result <- t a
                   if result
                     then test ts a
                     else return False

-- Tehtävä 10: Toteuta State-monadissa operaatio odds, joka tuottaa
-- tilan, jossa ovat kaikki ne alkiot jotka esiintyvät alkuperäisessä
-- listassa _parittoman_määrän_ kertoja.
--
-- (Tuotetun listan järjestyksellä ei ole väliä, testit järjestävät
-- listan ennen vertailua.)
--
-- Esimerkkejä:
--  runState (odds [1,2,3,1,2,1]) []
--    ==> ((),[1,3])
--  runState (odds [1,2,3,1,2,3,1,2,3]) []
--    ==> ((),[3,2,1])

odds :: Eq a => [a] -> State [a] ()
odds [] = return ()
odds (x:xs) = do occ <- get
                 put (toggle x occ)
                 odds xs
  where toggle x xs = if x `elem` xs then delete x xs else x:xs
