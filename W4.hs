module W4 where

-- ATTENZION muutama tämän viikon tehtävistä on kahden pisteen
-- arvoisia! Näistä tehtävistä ei kuitenkaan jaeta osittaisia pisteitä
-- vaan ne ovat "kaikki tai ei mitään".

-- Tehtävä 1: Toteuta funktio safeDiv, joka suorittaa kokonaislukujen
-- jakolaskun, mutta paluutyyppinä on Maybe Integer. Jos jakaja (eli
-- toinen argumentti) on 0, palautta funktio Nothing. Muuten funktio
-- palauttaa Just a, missä a on jakolaskun lopputulos.
--

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv x 0 = Nothing
safeDiv x y = Just (x `div` y)

-- Tehtävä 2: Tässä tehtävässä toteutetaan funktio eitherDiv, joka
-- toimii hieman kuten safeDiv, mutta palauttaa arvon tyyppiä Either
-- String Int.
--
-- Onnistuneen jakolaskun tapauksessa eitherDiv palauttaa Right x,
-- missä x on jakolaskun tulos. Jos jakaja on nolla, palauttaa
-- eitherDiv arvon Left "x/0", jotta kutsuja tietää mitä lukua
-- koitettiin jakaa nollalla.

eitherDiv :: Integer -> Integer -> Either String Integer
eitherDiv x 0 = Left (show x ++ "/0")
eitherDiv x y = Right (x `div` y)

-- Tehtävä 3: Toteuta funktio mapMaybe, joka toimii hieman kuten
-- yhdistetty map & filter.

-- mapMayben argumenttina oleva funktio on tyyppiä a -> Maybe b. Tätä
-- funktiota kutsutaan jokaiselle syötelistan alkiolle. Jos tulos on
-- Nothing, ei tuloslistaan tule mitään. Jos tulos on Just y,
-- tuloslistaan tulee arvo y.
--
-- Esimerkkejä:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in mapMaybe f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- mapMaybe Just [1,2,3]
--   ==> [1,2,3]
--
-- mapMaybe (\x -> Nothing) [1,2,3]
--   ==> []

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f [] = []
mapMaybe f (x:xs) = case f x of Just a -> a : mapMaybe f xs
                                Nothing -> mapMaybe f xs

-- Tehtävä 4: Toteuta funktio classify, joka saa listan arvoja tyyppiä
-- Either a b ja jakaa tuottaa näistä listan tyypin a arvoja ja listan
-- tyypin b arvoja.
--
-- PS. Tämä funktio löytyy standardikirjastosta nimellä
-- partitionEithers, älä kuitenkaan käytä tätä funktiota
-- toteutuksessasi (taikka mitään muuta funktiota modulista
-- Data.Either).
--
-- Esimerkkejä:
--  classify [Left 1, Right True, Left 0, Right False]
--     ==> ([1,0],[True,False])

classify :: [Either a b] -> ([a],[b])
classify es = (mapMaybe getLeft es, mapMaybe getRight es)
  where getLeft (Left l) = Just l
        getLeft _ = Nothing
        getRight (Right r) = Just r
        getRight _ = Nothing

-- Tehtävät 5&6: Määrittele tietotyyppi Person, joka sisältää yhden
-- Int-tyyppisen kentän (ikä) ja String-tyyppisen kentän (nimi).
--
-- Määrittele myös Person-arvo matti ja Person-tyypin käsittelemiseen
-- operaatiot getAge, getName, setAge ja setName. (Ks. alla)

data Person = Person Int String
  deriving Show

-- matti on henkilö jonka nimi on "Matti" ja ikä 90
matti :: Person
matti = Person 90 "Matti"

-- getName palauttaa henkilön nimen
getName :: Person -> String
getName (Person age name) = name

-- getAge palauttaa henkilön iän
getAge :: Person -> Int
getAge (Person age name) = age

-- setName asettaa henkilön nimen
-- HUOM! setName palauttaa uuden henkilön sillä Haskellissa mikään ei muutu
setName :: String -> Person -> Person
setName name (Person age _) = Person age name

-- setAge asettaa henkilön iän
setAge :: Int -> Person -> Person
setAge age (Person _ name) = Person age name
  

-- Tehtävä 7&8: Määrittele tietotyyppi TwoCounters joka esittää kahta
-- laskuria. Määrittele lisäksi alla listatut funktiot TwoCountersin
-- käsittelemiseen.
--
-- Esimerkkejä:
--
-- getA (incA (incA zeros))
--   ==> 2
-- getB (incB (incA zeros))
--   ==> 1

data TwoCounters = TwoCounters Int Int

-- zeros on TwoCounters-arvo, jossa kummatkin laskurit ovat 0
zeros :: TwoCounters
zeros = TwoCounters 0 0

-- getA palauttaa A-laskurin arvon
getA :: TwoCounters -> Int
getA (TwoCounters a _) = a

-- getB palauttaa B-laskurin arvon
getB :: TwoCounters -> Int
getB (TwoCounters _ b) = b

-- incA kasvattaa A-laskurin arvoa yhdellä
incA :: TwoCounters -> TwoCounters
incA (TwoCounters a b) = TwoCounters (a+1) b

-- incB kasvattaa B-laskurin arvoa yhdellä
incB :: TwoCounters -> TwoCounters
incB (TwoCounters a b) = TwoCounters a (b+1)

-- Tehtävä 9&10: Määrittele tietotyyppi UpDown joka esittää laskuria,
-- joka voi olla joko nousevassa tai laskevassa tilassa. Toteuta myös
-- alla kuvatut funktiot zero, toggle, tick ja get
--
-- HUOM! Käytä _kahta_ tyyppikonstruktoria! Ts. määritelmäsi tulee olla muotoa
--   data UpDown = A jotain | B jotain
--
-- Esimerkkejä:
--
-- get (tick zero)
--   ==> 1
-- get (tick (tick zero))
--   ==> 2
-- get (tick (tick (toggle (tick zero))))
--   ==> -1

data UpDown = Up Int | Down Int

-- zero on nouseva laskuri jonka arvo on 0
zero :: UpDown
zero = Up 0

-- get palauttaa laskurin arvon
get :: UpDown -> Int
get (Up i) = i
get (Down i) = i

-- tick kasvattaa nousevaa laskuria yhdellä ja pienentää laskevaa
-- laskuria yhdellä
tick :: UpDown -> UpDown
tick (Up i) = Up (i + 1)
tick (Down i) = Down(i - 1)

-- toggle muuttaa nousevan laskurin laskevaksi ja päinvastoin.
-- Laskurin arvo ei muutu.
toggle :: UpDown -> UpDown
toggle (Up i) = Down i
toggle (Down i) = Up i

-- !!!!!
-- Muutama seuraava tehtävä käsittelevät luennoilla esiteltyä
-- Tree-tyyppiä, jonka määritelmä on alla.

data Tree a = Leaf | Node a (Tree a) (Tree a)
            deriving (Show, Eq)
                     
-- Tehtävä 11: Toteuta funktio valAtRoot, joka palauttaa puun juuressa
-- (eli ylimmässä solmussa) olevan arvon. Paluutyyppinä on Maybe a
-- koska puu saattaa olla tyhjä (eli Leaf).

valAtRoot :: Tree a -> Maybe a
valAtRoot Leaf = Nothing
valAtRoot (Node val t1 t2) = Just val

-- Tehtävä 12: Toteuta funktio treeSize, joka laskee puun solmujen (eli
-- Node-konstruktorien) lukumäärän.

treeSize :: Tree a -> Int
treeSize (Leaf) = 0
treeSize (Node val t1 t2) = 1 + treeSize t1 + treeSize t2

-- Tehtävä 13: Toteuta funktio leftest, joka palauttaa puun
-- vasemmanpuolimmaisen arvon. Palautustyyppi on Maybe a koska
-- tyhjälle puulle (pelkkä Leaf) palautetaan Nothing.
--
-- Vasemmanpuolimmaisin arvo tarkoittaa sen solmun arvoa, johon pääsee
-- juuresta pelkästään vasempiin lapsiin kulkemalla.
--
-- Esimerkkejä:
-- leftest Leaf
--   ==> Nothing
-- leftest (Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) Leaf)
--   ==> Just 3
-- leftest (Node 1 (Node 2 Leaf (Node 3 Leaf Leaf)) (Node 4 Leaf Leaf))
--   ==> Just 2

leftest :: Tree a -> Maybe a
leftest (Leaf) = Nothing
leftest (Node val t1 t2) = case t1 of Node v a b -> leftest t1
                                      Leaf -> Just val

-- Tehtävä 14: Toteuta funktio mapTree, joka toimii kuten map, mutta
-- puille.
-- 
-- Esimerkkejä:
--
-- mapTree (+1) Leaf  ==>  Leaf
-- mapTree (+2) (Node 0 (Node 1 Leaf Leaf) (Node 2 Leaf Leaf))
--   ==> (Node 2 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf) = Leaf
mapTree f (Node val t1 t2) = Node (f val) (mapTree f t1) (mapTree f t2)

-- Tehtävä 15: Toteuta funktio insertL, joka lisää annetun arvon puuhun
-- mahdollisimman vasemmalle.
-- 
-- Mahdollisimman vasemmalle tarkoittaa tässä sitä, että annettuun
-- arvoon pitäisi päästä lisäyksen jälkeen juuresta pelkkiä vasempia
-- osoittimia seuraamalla.
--
-- Esimerkkejä:
-- insertL 0 Leaf
--   ==> Node 0 Leaf Leaf
-- insertL 0 (Node 1 Leaf Leaf)
--   ==> Node 1 (Node 0 Leaf Leaf) Leaf)
--
-- insertL 0 (Node 1
--             (Node 2
--               Leaf
--               (Node 3 Leaf Leaf))
--             (Node 4 Leaf Leaf))
--
--        ==> Node 1 
--             (Node 2
--               (Node 0 Leaf Leaf)
--               (Node 3 Leaf Leaf))
--             (Node 4 Leaf Leaf)


insertL :: a -> Tree a -> Tree a
insertL x (Leaf) = Node x Leaf Leaf
insertL x (Node val t1 t2) = Node val (insertL x t1) t2

-- Tehtävä 16: Toteuta funktio measure, joka muuntaa annetun puun
-- sellaiseksi, että jokaisessa solmussa on ko. solmusta alkavan
-- alipuun koko.
--
-- Puun koko tarkoittaa samaa kuin tehtävässä 12, eli
-- Node-konstruktorien lukumäärää.
--
-- Esimerkkejä:
--
-- measure (Node 0 Leaf Leaf)
--  ==> Node 1 Leaf Leaf
-- measure (Node 0 (Node 0 Leaf Leaf) Leaf)
--  ==> Node 2 (Node 1 Leaf Leaf) Leaf
-- measure (Node 0 (Node 0 Leaf Leaf) Leaf)
--  ==> Node 3 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf)
-- measure (Node 0 (Node 0 Leaf Leaf)
--                 (Node 0 (Node 0 Leaf Leaf)
--                         (Node 0 Leaf
--                                 (Node 0 Leaf Leaf))))
--      ==> Node 6 (Node 1 Leaf Leaf)
--                 (Node 4 (Node 1 Leaf Leaf)
--                         (Node 2 Leaf
--                                 (Node 1 Leaf Leaf)))


measure :: Tree a -> Tree Int
measure (Leaf) = Leaf
measure node@(Node val t1 t2) = Node (treeSize(node)) (measure t1) (measure t2)

-- Tehtävä 17: Standardikirjaston funktio
--   foldr :: (a -> b -> b) -> b -> [a] -> b
-- on tarkoitettu listan "romauttamiseen". Se toimii jotakuinkin näin:
--   foldr f start [x,y,z,w]
--     ==> f x (f y (f z (f w start)
--
-- Toteuta funktiot sumf ja lengthf siten, että mysum laskee listan
-- summan ja mylength laskee listan pituuden.
--
-- ÄLÄ siis koske funktioitten mysum ja mylength määritelmiin

mysum :: [Int] -> Int
mysum is = foldr sumf 0 is

sumf :: Int -> Int -> Int
sumf x y = x + y

mylength :: [a] -> Int
mylength xs = foldr lengthf 0 xs

lengthf :: a -> Int -> Int
lengthf x y = y + 1

-- Tehtävä 18: Toteuta funktio foldTree, joka on foldin tapainen
-- operaatio puille (eli yllä kuvaillulle Tree-luokalle).
--
-- Kun olet toteuttanut foldTreen oikein, alla olevat treeSum ja
-- treeLeaves -funktiot toimivat oikein.

sumt :: Int -> Int -> Int -> Int
sumt x y z = x+y+z

-- Laskee puun summan
treeSum :: Tree Int -> Int
treeSum t = foldTree sumt 0 t

leaft :: a -> Int -> Int -> Int
leaft x y z = y+z

-- Laskee puun lehdet
treeLeaves :: Tree a -> Int
treeLeaves t = foldTree leaft 1 t

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree f t Leaf = t
foldTree f t (Node val t1 t2) = f val (foldTree f t t1) (foldTree f t t2)

-- Tehtävä 19: Alla näet hieman laajennetun version luentojen
-- värityypistä Color.
-- 
-- Konstruktorin Mix tulkinta on, että se on kahden värin sekoitus.
--
-- Konstruktori Darken taas esittää yhden värin tummentamista.
-- Double-kenttä kertoo paljonko väriä tummennetaan, 0 tarkoittaa ei
-- tummennusta ja 1 tarkoittaa täysi tummennus.
--
-- Toteuta funktio rgb :: Color -> [Double] joka tuottaa kolmen
-- mittaisen double-listan joka esittää annettua väriä
-- rgb-avaruudessa. Funktion tulee toimia näin:
--
-- rgb Red   ==> [1,0,0]
-- rgb Green ==> [0,1,0]
-- rgb Blue  ==> [0,0,1]
-- 
-- rgb (Mix Red Green)                ==> [1,1,0]
-- rgb (Mix Red (Mix Red Green))      ==> [1,1,0]
-- rgb (Darken 0.2 Red)               ==> [0.8,0,0]
-- rgb (Darken 0.2 (Darken 0.2 Red))  ==> [0.64,0,0]
-- rgb (Mix (Darken 0.4 Red) (Darken 0.4 Red)) ==> [1,0,0]
-- rgb (Mix (Darken 0.6 Red) (Darken 0.6 Red)) ==> [0.8,0,0]
--
-- Mix siis kääntyy värivektorien saturoivaksi yhteenlaskuksi ja
-- Darken värivektorin skaalaamiseksi

data Color = Red | Green | Blue | Mix Color Color | Darken Double Color
  deriving Show

mixRgb :: [Double] -> [Double] -> [Double]
mixRgb a b = map saturatedAddition $ zip a b
  where saturatedAddition (a,b) = min 1 (a + b)

darkenRgb :: [Double] -> [Double] -> [Double]
darkenRgb xs ys = map saturatedSubtraction $ zip xs ys
  where saturatedSubtraction (a,b) = max 0 (a * b)

rgb :: Color -> [Double]
rgb Red = [1, 0, 0]
rgb Green = [0, 1, 0]
rgb Blue = [0, 0, 1]
rgb (Darken d c) = darkenRgb [1-d, 1-d, 1-d] $ rgb c
rgb (Mix a b) = mixRgb (rgb a) (rgb b)
