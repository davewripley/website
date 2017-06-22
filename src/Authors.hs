{-# LANGUAGE OverloadedStrings #-}
module Authors (Author(..), authors, makeAuthorLink) where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Lucid

data Author = Author { name :: Text, authorUrl  :: Text } deriving (Show, Read, Eq)
type AuthorMap = Map Text Author

makeAuthorLink :: Text -> Html ()
makeAuthorLink tg = case (authorUrl auth) of
                      "" -> toHtml (name auth)
                      x  -> a_ [href_ x, target_ "_blank"] (toHtml (name auth))
                    where
                      auth = fromJust (M.lookup tg authors)

authors :: AuthorMap
authors = M.fromList
  [("allenHazen"
   ,Author {name = "Allen P. Hazen"
           , authorUrl = ""})
  ,("davidRipley"
   ,Author {name = "David Ripley"
           , authorUrl = ""})
  ,("dominicHyde"
   ,Author {name = "Dominic Hyde"
           , authorUrl = "http://hapi.uq.edu.au/profile/388/dominic-hyde"})
  ,("edwinMares"
   ,Author {name = "Edwin Mares"
           , authorUrl = "http://www.victoria.ac.nz/hppi/about/staff/edwin-mares"})
  ,("ericMandelbaum"
   ,Author {name = "Eric Mandelbaum"
           , authorUrl = "http://www.ericmandelbaum.com"})
  ,("felipeDeBrigard"
   ,Author {name = "Felipe De Brigard"
           , authorUrl = "http://www.felipedebrigard.com/"})
  ,("franzBerto"
   ,Author {name = "Francesco Berto"
           , authorUrl = ""})
  ,("grahamPriest"
   ,Author {name = "Graham Priest"
           , authorUrl = "http://www.grahampriest.net"})
  ,("gregRestall"
   ,Author {name = "Greg Restall"
           , authorUrl = "http://consequently.org"})
  ,("jcBeall"
   ,Author {name = "Jc Beall"
           , authorUrl = "http://entailments.net"})
  ,("johnSlaney"
   ,Author {name = "John Slaney"
           , authorUrl = "http://users.cecs.anu.edu.au/~jks/"})
  ,("killripLemistery"
   ,Author {name = "Killrip Lemistery"
           , authorUrl = ""})
  ,("markColyvan"
   ,Author {name = "Mark Colyvan"
           , authorUrl = "http://www.colyvan.com"})
  ,("michaelDunn"
   ,Author {name = "J. Michael Dunn"
           , authorUrl = "http://www.indiana.edu/~phil/people/dunn.shtml"})
  ,("oleHjortland"
   ,Author {name = "Ole Hjortland"
           , authorUrl = "http://www.olehjortland.net"})
  ,("pabloCobreros"
   ,Author {name = "Pablo Cobreros"
           , authorUrl = "http://www.unav.es/adi/servlet/Cv2.ara?personid=74116"})
  ,("paulEgre"
   ,Author {name = "Paul Egr\233"
           , authorUrl = "http://paulegre.free.fr"})
  ,("richardSylvan"
   ,Author {name = "Richard Sylvan"
           , authorUrl = "http://en.wikipedia.org/wiki/Richard_Sylvan"})
  ,("robertMeyer"
   ,Author {name = "Robert K. Meyer"
           , authorUrl = "http://en.wikipedia.org/wiki/Bob_Meyer_%28logician%29"})
  ,("robertVanRooij"
   ,Author {name = "Robert van Rooij"
           , authorUrl = "http://www.uva.nl/over-de-uva/organisatie/medewerkers/content/r/o/r.a.m.vanrooij/r.a.m.van-rooij.html"})
  ,("rohanFrench"
   ,Author {name = "Rohan French"
           , authorUrl = "http://rohan-french.github.io/"})
  ,("rossBrady"
   ,Author {name = "Ross Brady"
           , authorUrl = "http://www.latrobe.edu.au/humanities/about/staff/profile?uname=RTBrady"})
  ,("samBaron"
   ,Author {name = "Sam Baron"
           , authorUrl = "http://sambaron.horse"})
  ,("vincentDeGardelle"
   ,Author {name = "Vincent de Gardelle"
           , authorUrl = "https://sites.google.com/site/vincentdegardelle/home"})
  ,("zachWeber"
   ,Author {name = "Zach Weber"
           , authorUrl = "https://sites.google.com/site/doctorzachweber/home"})
  ]