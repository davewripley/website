{-# LANGUAGE OverloadedStrings #-}

module Links where

import Lucid
import Data.Text (Text)

import WebsiteTools (lk)

aalLink :: Html () -> Html ()
aalLink = lk "http://aal.ltumathstats.com"

aalLinkFull :: Html ()
aalLinkFull = aalLink "AAL"

aapLink :: Html () -> Html ()
aapLink = lk "http://www.aap-conferences.org.au/"

aapLinkFull :: Html ()
aapLinkFull = aapLink "AAP"

ajlLink :: Html () -> Html ()
ajlLink = lk "http://ojs.victoria.ac.nz/ajl"

aslLink :: Html () -> Html ()
aslLink = lk "http://www.aslonline.org"

emailLink :: Html () -> Html ()
emailLink = lk "mailto:davewripley@gmail.com"

ijnLink :: Html () -> Html ()
ijnLink = lk "http://www.institutnicod.org"

ijnLinkFull :: Html ()
ijnLinkFull = ijnLink "Insitut Jean Nicod"

mcmpLink :: Html () -> Html ()
mcmpLink = lk "http://www.mcmp.philosophie.uni-muenchen.de/index.html"

mcmpLinkFull :: Html ()
mcmpLinkFull = mcmpLink "MCMP"

openAccessLink :: Html () -> Html ()
openAccessLink = lk "http://www.jasonmkelly.com/2013/01/27/green-gold-and-diamond-a-short-primer-on-open-access"

rslLink :: Html () -> Html ()
rslLink = lk "http://journals.cambridge.org/RSL"

twitterLink :: Html () -> Html ()
twitterLink = lk "http://twitter.com/davewripley"

uconnLink :: Html () -> Html ()
uconnLink = lk "http://www.uconn.edu"

uconnLogicLink :: Html () -> Html ()
uconnLogicLink = lk "http://logic.uconn.edu"

uconnPhilLink :: Html () -> Html ()
uconnPhilLink = lk "http://philosophy.uconn.edu"
