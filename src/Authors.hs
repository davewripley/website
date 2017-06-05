{-# LANGUAGE OverloadedStrings #-}
module Authors (Author(..), authors, makeAuthorLink) where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Lucid

data Author = Author { name :: Text, url  :: Text } deriving (Show, Read, Eq)
type AuthorMap = Map Text Author

makeAuthorLink :: Text -> Html ()
makeAuthorLink tg = case (url auth) of
                      "" -> toHtml (name auth)
                      x  -> a_ [href_ x, target_ "_blank"] (toHtml (name auth))
                    where
                      auth = fromJust (M.lookup tg authors)

authors :: AuthorMap
authors = M.fromList
  [("allenHazen"
   ,Author {name = "Allen P. Hazen"
           , url = ""})
  ,("davidRipley"
   ,Author {name = "David Ripley"
           , url = ""})
  ,("dominicHyde"
   ,Author {name = "Dominic Hyde"
           , url = "http://hapi.uq.edu.au/profile/388/dominic-hyde"})
  ,("edwinMares"
   ,Author {name = "Edwin Mares"
           , url = "http://www.victoria.ac.nz/hppi/about/staff/edwin-mares"})
  ,("ericMandelbaum"
   ,Author {name = "Eric Mandelbaum"
           , url = "http://www.ericmandelbaum.com"})
  ,("felipeDeBrigard"
   ,Author {name = "Felipe De Brigard"
           , url = "http://www.felipedebrigard.com/"})
  ,("grahamPriest"
   ,Author {name = "Graham Priest"
           , url = "http://www.grahampriest.net"})
  ,("gregRestall"
   ,Author {name = "Greg Restall"
           , url = "http://consequently.org"})
  ,("jcBeall"
   ,Author {name = "Jc Beall"
           , url = "http://entailments.net"})
  ,("johnSlaney"
   ,Author {name = "John Slaney"
           , url = "http://users.cecs.anu.edu.au/~jks/"})
  ,("killripLemistery"
   ,Author {name = "Killrip Lemistery"
           , url = ""})
  ,("markColyvan"
   ,Author {name = "Mark Colyvan"
           , url = "http://www.colyvan.com"})
  ,("michaelDunn"
   ,Author {name = "J. Michael Dunn"
           , url = "http://www.indiana.edu/~phil/people/dunn.shtml"})
  ,("oleHjortland"
   ,Author {name = "Ole Hjortland"
           , url = "http://www.olehjortland.net"})
  ,("pabloCobreros"
   ,Author {name = "Pablo Cobreros"
           , url = "http://www.unav.es/adi/servlet/Cv2.ara?personid=74116"})
  ,("paulEgre"
   ,Author {name = "Paul Egr\233"
           , url = "http://paulegre.free.fr"})
  ,("richardSylvan"
   ,Author {name = "Richard Sylvan"
           , url = "http://en.wikipedia.org/wiki/Richard_Sylvan"})
  ,("robertMeyer"
   ,Author {name = "Robert K. Meyer"
           , url = "http://en.wikipedia.org/wiki/Bob_Meyer_%28logician%29"})
  ,("robertVanRooij"
   ,Author {name = "Robert van Rooij"
           , url = "http://www.uva.nl/over-de-uva/organisatie/medewerkers/content/r/o/r.a.m.vanrooij/r.a.m.van-rooij.html"})
  ,("rohanFrench"
   ,Author {name = "Rohan French"
           , url = "http://rohan-french.github.io/"})
  ,("rossBrady"
   ,Author {name = "Ross Brady"
           , url = "http://www.latrobe.edu.au/humanities/about/staff/profile?uname=RTBrady"})
  ,("samBaron"
   ,Author {name = "Sam Baron"
           , url = "http://sambaron.horse"})
  ,("vincentDeGardelle"
   ,Author {name = "Vincent de Gardelle"
           , url = "https://sites.google.com/site/vincentdegardelle/home"})
  ,("zachWeber"
   ,Author {name = "Zach Weber"
           , url = "https://sites.google.com/site/doctorzachweber/home"})
  ]