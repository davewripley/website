{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module Website (websiteMain) where

import Data.Monoid ((<>), mempty, mconcat)
import Data.Text (Text)
import Data.Text.Lazy.IO (writeFile)
import Data.List (intersperse, sortBy)
import Lucid
import Lucid.Bootstrap

import WebsiteTools (AuthorCat(..), Parity(..), classify, listItems, pileUp, lk)
import Links
import Authors (Author, authors, makeAuthorLink)
import Writing (writing, Piece, pieceTitle, pieceAuthorTags, pieceUrl, pieceVenue, pieceYear, pieceAuthorCat)
import Presentations (Presentation(..), extrasMarks, presentations, presLinkList)


--SECTION: General template pieces

htmlHeadBits :: Html ()
htmlHeadBits = meta_ [charset_ "utf-8"]
               <> meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge"]
               <> meta_ [name_ "viewport", content_ "width=device-width, intial-scale=1"]
               <> meta_ [name_ "description", content_ "David Ripley's website"]
               <> meta_ [name_ "author", content_ "David Ripley"]
               <> title_ "David Ripley"
               <> link_ [rel_ "stylesheet", href_ "./css/bootstrap.min.css"]
               <> link_ [rel_ "stylesheet", href_ "./font-awesome-4.3.0/css/font-awesome.min.css"]
               <> link_ [rel_ "stylesheet", href_ "./css/ripley.css"]


pageHeader :: Html ()
pageHeader =
  div_ [class_ "header"] $
    container_ $
      row_ $
        nav_ [class_ "navbar navbar-fixed-top navbar-inverse"] $
          container_ $ do
            div_ [id_ "navbar", class_ "navbar-collapse collapse"]
              (ul_ [class_ "nav navbar-nav"]
                   ((li_ [class_ "navbar-header indexlink"] (a_ [class_ "navbar-brand", href_ "./index.html"] "David Ripley"))
                 <> (li_ [class_ "writinglink"] (a_ [href_ "./writing.html"] "Writing"))
                 <> (li_ [class_ "presentationlink"] (a_ [href_ "./presentations.html"] "Presentations"))  
                 <> (li_ [class_ "teachinglink"] (a_ [href_ "./teaching.html"] "Teaching"))
                 <> (li_ [class_ "cvlink"] (a_ [href_ "./ripleyCV.pdf", target_ "_blank"] "CV"))))


pageFooter :: Html ()
pageFooter =
  div_ [class_ "footer"] $
    container_ $
      row_ $ do
        div_ [class_ "col-md-4"]
          (table_ $ do
            (tr_ $ do
              (td_ [class_ "contact-icon"] (span_ [class_ "fa fa-fw fa-inbox"] ""))
              (td_ (emailLink "davewripley@gmail.com")))
            (tr_ $ do 
              (td_ [class_ "contact-icon"] (span_ [class_ "fa fa-fw fa-twitter"] ""))
              (td_ (twitterLink "@davewripley"))))
        div_ [class_ "col-md-4"]
          (table_ $ do
            (tr_ $ do
              (td_ [class_ "contact-icon"] (span_ [class_ "fa fa-fw fa-envelope"] ""))
              (td_ (uconnPhilLink "Department of Philosophy")))
            (tr_ $ do
              (td_ [class_ "contact-icon"] "")
              (td_ (uconnLink "University of Connecticut")))
            (tr_ $ do
              (td_ [class_ "contact-icon"] "")
              (td_ (p_ [class_ "address"] "101 Manchester Hall")))
            (tr_ $ do
              (td_ [class_ "contact-icon"] "")
              (td_ (p_ [class_ "address"] "344 Mansfield Rd")))
            (tr_ $ do
              (td_ [class_ "contact-icon"] "")
              (td_ (p_ [class_ "address"] "Storrs, CT 06269 USA"))))
        div_ [class_ "col-md-4"]
          (table_ $ do
            (tr_ $ do
              (td_ [class_ "contact-icon"] (span_ [class_ "fa fa-fw fa-smile-o"] ""))
              (td_ (p_ ("Site made with "
                      <> (lk "https://www.haskell.org" "Haskell")
                      <> ", "
                      <> (lk "https://github.com/chrisdone/lucid" "Lucid")
                      <> ", "
                      <> (lk "http://getbootstrap.com" "Bootstrap")
                      <> ", "
                      <> (lk "http://jquery.com" "jQuery")
                      <> ", "
                      <> (lk "http://fontawesome.io" "Font Awesome"))))))
                 

scriptImports :: Html ()
scriptImports = do
  script_ [src_ "./js/jquery-2.1.3.min.js"] ""
  script_ [src_ "./js/bootstrap.min.js"] ""

pageFrom :: Html () -> Html() -> Html ()
pageFrom bod scrip = doctypehtml_
    (head_ htmlHeadBits) <> (body_ (pageHeader <> bod <> pageFooter <> scriptImports <> scrip))

navbarJS :: Text -> Html ()
navbarJS t = script_ [type_ "text/javascript"]
  ("var setActive = function () {\n$(\"." <> t <> "\").addClass(\"active\");\n};\n$(document).ready(setActive);")


--SECTION: Index page

indexPage :: Html ()
indexPage = pageFrom indexBody (navbarJS "indexlink")

indexBody :: Html ()
indexBody =
  div_ [class_ "mainbits"] $
    container_ $
      row_ $ do
        div_ [class_ "col-md-6"] (img_ [class_ "img-rounded splashimg", src_ "./rockandroll.jpg"])
        div_ [class_ "col-md-6 mainbits"] indexBodyText

indexBodyText :: Html ()
indexBodyText =
     h1_ [class_ "good-morning"] "Good morning!"
  <> p_ ("I'm David Ripley, a member of the "
        <> uconnPhilLink "philosophy department"
        <> " at the University of Connecticut.")
  <> p_ "My research focuses on language, logic, and the relations between them."
  <> p_ ("I'm also a member of the "
        <> uconnLogicLink "UConn Logic Group"
        <> " and the "
        <> aalLink "Australasian Association for Logic.")
  <> p_ ("I serve as an editor for two journals: the "
        <> ajlLink "Australasian Journal of Logic"
        <> ", a "
        <> openAccessLink "diamond open-access"
        <> " journal for logic of all sorts, and the "
        <> rslLink "Review of Symbolic Logic"
        <> ", a journal of the "
        <> aslLink "ASL"
        <> ".")
  <> p_ ("You can email me at "
        <> emailLink "davewripley@gmail.com"
        <> ".")


--SECTION: teaching page

teachingPage :: Html ()
teachingPage = pageFrom teachingBody (navbarJS "teachinglink")

classRow :: Parity -> Html () -> Html () -> [Html ()] -> Html ()
classRow par classNum className semesters =
  row_ [class_ (classify par)] $ do
    div_ [class_ "col-md-2"] (p_ [class_ "talktitle"] classNum)
    div_ [class_ "col-md-5"] (p_ [class_ "talktitle"] className)
    div_ [class_ "col-md-5"] (ul_ (listItems [] semesters))

classes :: [(Html (), Html (), [Html ()])]
classes = [("Phil 1102", "Philosophy and logic", ["Spring 2016"
                                                 ,"Fall 2014"
                                                 ,"Spring 2013"
                                                 ])
          ,("Phil 1105", "Philosophy of religion", ["Fall 2015"
                                                   ,"Fall 2013"
                                                   ])
          ,("Phil 2210", "Metaphysics and epistemology", ["Fall 2013"])
          ,("Phil 2211Q", "Symbolic Logic I", ["Spring 2016"])
          ,("Phil 3241", "Philosophy of language", ["Fall 2015"])
          ,("Phil 5344", "Seminar in philosophical logic", ["Spring 2014"])
          ,("Phil 5397", "Seminar in probability", [a_ [href_ "./fall_2014/phil5397.html"] "Fall 2014"])
          ]

teachingBody :: Html ()
teachingBody =  
  div_ [class_ "mainbits"] $
       topLabel "Teaching"
    <> (container_ $ pileUp (zipWith ($) rowCycle classes))
  where
    rowCycle = cycle [uncurry3 (classRow Odd), uncurry3 (classRow Even)]
    uncurry3 f = \(a,b,c) -> f a b c

--SECTION: presentation page

presentationPage :: Html ()
presentationPage = pageFrom presentationBody (navbarJS "presentationlink")

topLabel :: Html () -> Html ()
topLabel lab = container_ (h1_ [class_ "toplabel"] lab)


presentationAuthors :: AuthorCat -> Html ()
presentationAuthors Solo = mempty
presentationAuthors CERvR = presentationAuthors (Other [ "pabloCobreros", "paulEgre", "davidRipley", "robertVanRooij" ])
presentationAuthors (Other as) = p_ [class_ "presentation-authors" ] (mconcat $ intersperse ", " (map makeAuthorLink as))


presRow :: Presentation -> Html ()
presRow p =
  row_ [class_ "presentation-row"] $ do
    div_ [class_ "col-md-11 pres-bubble"] $ do
        div_ [class_ "col-md-5"]
            ((p_ [class_ "talktitle"] (toHtml $ presTitle p))
             <> presentationAuthors (presAuthors p))
        div_ [class_ "col-md-7"]
             (ul_ [class_ "presentation-venue"]
             (listItems [class_ "presentation-venue"] (presLocations p)))
        presLinkList p
    extrasMarks p


presentationBody :: Html ()
presentationBody = do
  topLabel "Presentations"
  container_ $ do
    div_ [class_ "mainbits"] $ do
        pileUp (map presRow presentations)
        


--SECTION: writing page

searchJS :: Html ()
searchJS = script_ [src_ "./js/search.js"] ""

writingPage :: Html ()
writingPage = pageFrom writingBody (navbarJS "writinglink" <> searchJS)

searchBar :: Html ()
searchBar = div_ [class_ "input-group"] $ do
  (span_ [class_ "input-group-addon"] (span_ [class_ "fa fa-search"] ""))
  (input_ [class_ "form-control", id_ "title-search-box", type_ "text", placeholder_ "Title search"])

searchSort :: Html ()
searchSort = mempty

searchFilters :: Html ()
searchFilters = div_ $ do
  (h6_ [class_ "filterhead"] "Filter by author:")
  (form_ [action_ ""] $ do
    (p_ [class_ "searchcheck"] $ (input_ [type_ "checkbox", name_ "check-solo"]) <> " Just Dave")
    (p_ [class_ "searchcheck"] $ (input_ [type_ "checkbox", name_ "check-cervr"]) <> " CERvR "
      <> (a_ [ (term "tabindex") "0"
             , (term "data-toggle") "popover"
             , (term "data-trigger") "hover"
             , title_ "CERvR is:"
             , (term "data-html") "true"
             , (term "data-content") "Pablo Cobreros, <br> Paul Egré, <br> David Ripley, <br> Robert van Rooij"
             ] "[?]"))
    (p_ [class_ "searchcheck"] $ (input_ [type_ "checkbox", name_ "check-other"]) <> " Other combinations"))

searchReset :: Html ()
searchReset =
  button_ [class_ "btn btn-default", term "role" "button"]
    (span_ [class_ "fa fa-asterisk"] "" <> " Show all")

philpapersBit :: Html ()
philpapersBit = p_ [class_ "philpapers"] ("Also see my " <> (lk "http://philpapers.org/profile/12303" "philpapers profile") <> ".")

writingBody :: Html ()
writingBody = do
    topLabel "Writing"
    container_ [class_ "mainbits"] $ 
      row_ $ do
        div_ [class_ "col-md-3 searchbar"]
            (searchBar <> searchSort <> searchFilters <> philpapersBit)
        div_ [class_ "col-md-9 searchresults"]
            (ul_ [class_ "writingdisplay"] (pileUp $ map makeEntry (sortBy pieceSort writing)))

paperTitleHead :: Piece -> Html ()
paperTitleHead p =
  case (pieceUrl p) of
    "" -> pt
    u  -> a_ [ href_ u
             , class_ "title-link"
             , target_ "_blank"
             ] pt
  where pt = toHtml (pieceTitle p)

makeEntry :: Piece -> Html ()
makeEntry p = 
  let cls = "paperbubble " <> (classify $ pieceAuthorCat p)
      auths = map makeAuthorLink (pieceAuthorTags p)
  in li_ [class_ cls] $ do
         p_ [class_ "ptitle"] (paperTitleHead p)
         p_ [class_ "pauthors"] (mconcat $ intersperse ", " auths)
         p_ [class_ "pvenue"] (pieceVenue p)


pieceSort :: Piece -> Piece -> Ordering
pieceSort p1 p2 =
  case (pieceYear p1, pieceYear p2) of
    (Nothing, Nothing) -> nameSort p1 p2
    (Nothing, _)       -> LT
    (_      , Nothing) -> GT
    (Just y1, Just z1) -> case z1 `compare` y1 of
                            EQ -> nameSort p1 p2
                            x  -> x
  where
    nameSort p1 p2 = (pieceTitle p1) `compare` (pieceTitle p2)



  



--SECTION: generation

dirPrefix :: FilePath
dirPrefix = "./for-upload/"

websiteMain :: IO ()
websiteMain = do
  Data.Text.Lazy.IO.writeFile (dirPrefix <> "index.html") (renderText indexPage)
  Data.Text.Lazy.IO.writeFile (dirPrefix <> "teaching.html") (renderText teachingPage)
  Data.Text.Lazy.IO.writeFile (dirPrefix <> "presentations.html") (renderText presentationPage)
  Data.Text.Lazy.IO.writeFile (dirPrefix <> "writing.html") (renderText writingPage)
