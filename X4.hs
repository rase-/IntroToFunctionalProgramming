{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, OverlappingInstances, UndecidableInstances #-}

module X4 where

-- Tehtävä 1&2: Määrittele newtype nimeltään LexList joka ottaa yhden
-- tyyppiparametrin a ja sisältää listan a-tyyppisiä arvoja. Toteuta
-- LexListille Eq-instanssi joka toimii kuten listojen Eq-instanssi
-- (vertaillaan kaikki alkioita pareittain), ja myös Ord-instanssi.
--
-- Ord-instanssin on kuitenkin tarkoitus vertailla listoja
-- _leksikografisessa_ järjestyksessä, eli lyhyempi lista tulee aina
-- ennen pitempää, ja samanpituiset listat ovat normaalissa
-- järjestyksessä. Ord-instanssi siis järjestää listoja näin:
--
--   [1] < [0,0]
--   [0,0] < [0,1]
--   [0,0] < [1,1]

-- Tehtävä 3&4: Toteuta tyyppiluokka Find a b, joka ilmaisee, että
-- a-tyypin arvosta voidaan etsiä b-tyypin arvoja. Luokassa tulisi
-- olla ainakin funktio find :: a -> b -> Bool.
--
-- Toteuta myös seuraavat Find-instanssit:
--   - Eq a => Find a a     -- ovatko arvot samat
--   - Eq a => Find [a] a   -- esiintyykö arvo listassa
--   - Find [Char] [Char]   -- esiintyykö merkkijono toisen osana
--
-- Perustele mitä instanssia käytetään kussakin seuraavista
-- esimerkeistä (Huom! OverlappingInstances on käytössä!):
--   a) find "MOI" 'a'
--   b) find 'a'   'b'
--   c) find "MOI" "OI"
--   d) find [1,2,3] []
--   e) find [putChar 'a'] (putChar 'a')

-- Tehtävä 5: Tämä tehtävä käsittelee seuraavanlaista
-- merkkijonomuunnosta:
--
--  1. Merkkijonosta poistetaan lainausmerkit: '
--  2. Merkkijonosta poistetaan välilyönnit alusta ja lopusta
--  3. Merkkijono ympäröidään lainausmerkeillä: '
--
-- Muunnos etenisi siis esimerkiksi näin
-- "' abcd' " ==> " abcd " ==> "abcd" ==> "'abcd'"
-- 
-- On tärkeää, että kaikki muunnoksen vaiheet suoritetaan ja ne että
-- ne suoritetaan oikeassa järjestyksessä.
--
-- Tehtänäsi onkin toteuttaa funktiot
--
--   - wrap
--   - removeQuotes
--   - removeSpaces
--   - addQuotes
--   - unwrap
--
-- siten, ettei niitä voi ajaa väärässä järjestyksessä. Tähän
-- käytetään alla olevaa haamutyyppiä käyttävää tyyppiä WrappedString.
--
-- Esimerkkejä:

-- Näin funktioitten utlisi toimia:
t3e1 = unwrap (addQuotes (removeSpaces (removeQuotes (wrap "' abcd' "))))  -- ==> "'abcd'"
-- Seuraavien tulisi olla tyyppivirheitä:
--t3e2 = unwrap (wrap "moi")
--t3e3 = unwrap (removeQuotes (wrap "moi"))
--t3e4 = unwrap (removeSpaces (removeQuotes (wrap "moi")))
--t3e4 = unwrap (addQuotes (removeQuotes (wrap "moi")))
--t3e5 = unwrap (addQuotes (addQuotes (removeSpaces (removeQuotes (wrap "' abcd' ")))))
--t3e6 = unwrap (removeSpaces (wrap "moi"))
--t3e7 = unwrap (addQuotes (wrap "moi"))

data Start = Start
data Phase1 = Phase1
data Phase2 = Phase2
data Phase3 = Phase3

data WrappedString phase = Wrap String

wrap = undefined
unwrap = undefined
removeQuotes = undefined
removeSpaces = undefined
addQuotes = undefined


-- Tehtävä 6: Toteuta laskuri, jonka voi lukita niin ettei sen arvoa
-- voi enää muuttaa. Tee lukittu/lukitsematon erottelu haamutyypillä
-- kuten materiaalin laskuriesimerkissä.
--
-- Toteuta tyypillesi operaatiot:
--
--  newCounter -- uusi lukitsematon laskuri jonka arvo on 0
--  getVal -- palauttaa arvon
--  inc -- kasvattaa yhdellä
--  dec -- vähentää yhdellä
--  lock -- muuttaa (lukitsemattoman tai jo lukitun) laskurin lukituksi

-- Tehtävä 7&8: Alla luokka Identify, johon on liitetty tietotyyppi
-- Identifier. Ideana on että Identifier X on arvo jonka perusteella
-- osaamme tunnistaa tyyppiä X olevan arvon.

class Identify a where
  data Identifier a
  identify :: a -> Identifier a
  match :: Identifier a -> a -> Bool
  
-- Tässä Person-tyyppi. Toteuta instanssi Identify Person, joka
-- tunnistaa henkilön henkilötunnuksen (socialSecurity) perusteella.
  
data Person = Person {name :: String,
                      age :: String,
                      address :: String,
                      socialSecurity :: String}
              
-- Tässä Car-tyyppi. Toteuta instanssi Identify Car, joka tunnistaa
-- auton rekisterinumeron perusteella.

data Car = Car {brand :: String,
                model :: String,
                year :: Int,
                registry :: String}
           
-- Toteuta vielä instanssi Identify (a,b) joka tunnistaa parin
-- sen _ensimmäisen_ komponentin perusteella.
           
-- Toteuta lopuksi funktio searchI, joka etsii listasta annettua
-- Identifieriä vastaavan arvon.

searchI :: Identify a => Identifier a -> [a] -> Maybe a
searchI = undefined

-- Tehtävä 9: Toteuta Find-tyyppiluokka uudelleen luokkana Find2 a,
-- joka sisältää liitetyn tyypin (associated type) Findable a, joka
-- vastaa Find-luokan b-parametriä.
--
-- Toteuta joitakin instansseja Find2:sta. Muista että instanssit
-- eivät saa mennä laisinkaan päällekäin.

-- Tehtävä 10: Toteuta kivi-paperi-sakset pelin voittajan
-- määrittäminen tyyppijärjestelmässä.
--
-- PS. muista lukea NumFamily.hs:stä kuinka tyyppitason laskentaa voi
-- testata GHCi:ssä!

data Rock
data Paper
data Scissors

-- Tehtävä 11: Määrittele tyyppitason vähennyslasku NumFamily-modulin
-- tyyliin.