module X3 where

import Data.List

import MyIteratee

import qualified Data.Enumerator as E
import qualified Data.Map as M

-- Tämän viikon tehtävistä 2-6 on tarkoitus ratkaista luentojen
-- MyIteratee-toteutuksella. Tehtävät 7-11 sen sijaan käsittelevät
-- Enumerator-kirjastoa.

-- Tehtävä 1: Korvaa alla olevan undefinedit toteutuksilla niin että
-- funktiot evenLength ja myTake toimivat. ÄLÄ MUUTA itse funktioitten
-- evenLength ja myTake tyyppejä tai määritelmiä.
--
-- Huom! Tässä on siis kyse aivan normaaleista listafoldeista eikä
-- vielä mistään Iterateista.

-- Palauttaa True jos listan pituus on parillinen, muuten False
evenLength :: [a] -> Bool
evenLength xs = foldl' evenF evenStart xs

evenF = undefined -- Toteuta!
evenStart = undefined -- Toteuta!

-- Palauttaa listan k ensimmäistä alkiota, järjestyksessä
myTake :: Int -> [a] -> [a]
myTake k xs = foldl' (takeF k) takeStart xs

takeF = undefined -- Toteuta!
takeStart = undefined -- Toteuta!

-- Tehtävä 2: Toteuta Iteratee count, joka laskee saamiensa alkioitten
-- lukumäärän.
--
-- Esimerkkejä:
--  run $ foldList count [True,True,True,True,True,True,False]
--    ==> 7
--  run $ foldList count "abcde"
--    ==> 5

count :: Iteratee a Int
count = undefined

-- Tehtävä 3: Toteuta Iteratee iproduct, joka laskee saamiensa lukujen
-- tulon. iproductin tulee lopettaa syötteen kuluttaminen heti kun se
-- saa 0:n
--
-- Esimerkkejä:
--  run $ foldList iproduct [1,2,4]
--    ==> 8  
--  run $ foldList iproduct [1,2,4,0]
--    ==> 0
--  run $ foldList iproductAndCollect [1,2,4,0,9,8]
--    ==> (0,[9,8])

iproduct :: Iteratee Int Int
iproduct = undefined

-- Tällä voit testata että iproductisi jättää syötettä lukematta
-- sopivasti
iproductAndCollect = do p <- iproduct
                        rest <- collectAll
                        return (p,rest)

-- Tehtävä 4: Toteuta IO-operaatio foldFileLines, joka saa
-- argumentteinaan tiedostonimen ja Iterateen jonka syötetyyppi on
-- String. foldFileLines lukee tiedostoa rivi kerrallan ja syöttää
-- rivit annetulle iterateelle.
--
-- Esimerkki:
--
-- Tiedostossa foo on seuraavat rivit:
--
-- abc
-- defg
-- hi
--
-- Nyt voimme tehdä ghci:ssä näin:
--
-- X3> i <- foldFileLines collectAll "foo"
-- X3> run i
--
-- ja tulos on: ["abc","defg","hi"]

foldFileLines :: Iteratee String a -> FilePath -> IO (Iteratee String a)
foldFileLines iter fp = undefined

-- Tehtävä 5: Toteuta iteraattori readPersons, joka lukee
-- seuraavankaltaista formaattia:
--
-- 4
-- Pekka Pekkarinen 17
-- Mauri Gerdt 55
-- P Hofstadter 42
-- N N 100
--
-- Ensin siis on rivi joka kertoo monenko henkilön tiedot on luvassa.
-- Sen jälkeen seuraa henkilöitä, yksi per rivi. Jokainen henkilö
-- koostuu etunimestä, sukunimestä ja iästä.
--
-- readPersonsin tulee palauttaa lista kolmikkoja (String,String,Int),
-- jotka esittävät syötteessä olleita henkilöitä. Esimerkiksi
-- ylläolevalle syötteelle tuotettaisiin lista:
--
--  [("Pekka","Pekkarinen",17),("Mauri","Gerdt",55),("P","Hofstadter",42),("N","N",100)]
--
-- Vihje! Tässä kannattaa käyttää Iterateen Monad-instanssia ja
-- määritellä ensin pieniä apuiterateita kuten muistiinpanoissakin
-- tehdään.

readPersons :: Iteratee Char [(String,String,Int)]
readPersons = undefined

-- Tehtävä 6: Toteuta Enumeratee-kirjaston sequence-funktiota vastaava
-- asia meidän Iteratee-tyypillemme.
--
-- Esimerkkejä:
--  run $ foldList (mysequence readInt %% collectAll) "2 3 4 100"
--    ==> [2,3,4,100]
--  run $ foldList (mysequence readInt %% sumAtMost 3)
--    ==> 9

mysequence :: Iteratee outer inner -> Enumeratee outer inner a
-- mysequence :: Iteratee outer inner -> Iteratee inner a -> Iteratee outer (Iteratee inner a)
mysequence = undefined



------------------------------------------
-- Nyt vaihdamme Enumeratee-kirjastoon ---
------------------------------------------



-- Tehtävä 7: Toteuta samanlainen tulon laskeva iteratee kuin
-- tehtävässä 3, mutta tällä kertaa enumerator-kirjaston avulla.
--
-- Vihje: Muista lukea enumerator-neuvot muistiinpanoista

eproduct :: Monad m => E.Iteratee Int m Int
eproduct = undefined

-- Tehtävä 8: Toteuta iteratee joka laskee Data.Mappiin syötteen
-- merkkien esiintymislukumäärät. Ideana siis tehdä samaa kuin
-- ensimmäisen viikon Histo.hs.

histo :: Monad m => E.Iteratee Char m (M.Map Char Int)
histo = undefined

-- Tehtävä 9: Toteuta Iteratee printPersons joka luke samanlaista
-- formaattia kuin tehtävässä 5 mutta luettujen henkilöitten
-- palauttamisen sijaan tulostaa ne muodossa:
--
-- Pekkarinen, Pekka (17)
--
-- Tulostusten tulee tapahtua heti kun henkilö on saatu luettua, älä
-- siis kerää henkilöitä listaan tai mitään sellaista.
--
-- Muista: IO:n ajaminen Iterateen sisällä tehtiin funktiolla tryIO
--
-- Haaste! Tämän voi tehdä käyttäen Data.Enumerator.List.isolate ja
-- Data.Enumerator.List.sequence -funktioita käyttäen!

printPersons :: E.Iteratee Char IO ()
printPersons = undefined

-- Tehtävä 10: Toteuta Data.Enumerator.List.sequence-funktiota
-- käyttäen iteraattori, joka laskee syötteessä olevien rivien määrän.

countLines :: Monad m => E.Iteratee Char m Int
countLines = E.sequence undefined E.=$ undefined

-- Tehtävä 11: Toteuta Enumerator-kirjaston avulla IO-operaatio, joka
-- lukee tiedostosta rivejä, summaa riveillä olevat luvut, ja
-- kirjoittaa summat toiseen tiedostoon.
--
-- Vihje: Data.Enumerator.Text.enumFile ja iterHandle
-- 
-- Esimerkki:
--
-- Jos syötetiedostossa on:
--
-- 1 2 3
-- 4 5
-- 7
--
-- Tulee tulostiedostoon:
--
-- 6
-- 9
-- 7

doSums :: FilePath -> FilePath -> IO ()
doSums inFile outFile = undefined
