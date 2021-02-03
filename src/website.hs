{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module Website (websiteMain) where

import Data.Monoid ((<>), mempty, mconcat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.IO (writeFile)
import Data.List (intersperse, sortBy)
import System.Directory (createDirectoryIfMissing)
import Lucid
import Lucid.Bootstrap

import WebsiteTools (AuthorCat(..), classify, listItems, pileUp, lk)
import Links
import Authors (Author, authors, makeAuthorLink)
import Writing (pieces, WritingPiece(..), wpAuthorTags, wpVenue, wpBibtex)
import Presentations (Presentation(..), extrasMarks, presentations)


--SECTION: General template pieces

topLabel :: Html () -> Html ()
topLabel lab = container_ (h1_ [class_ "toplabel"] lab)


htmlHeadBits :: Html ()
htmlHeadBits = meta_ [charset_ "utf-8"]
               <> meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge"]
               <> meta_ [name_ "viewport"
                        , content_ "width=device-width, intial-scale=1"]
               <> meta_ [name_ "description", content_ "David Ripley's website"]
               <> meta_ [name_ "author", content_ "David Ripley"]
               <> title_ "David Ripley"
               <> link_ [rel_ "stylesheet", href_ "./css/bootstrap.min.css"]
               <> link_ [rel_ "stylesheet"
                        , href_ "./font-awesome-4.3.0/css/font-awesome.min.css"]
               <> link_ [rel_ "stylesheet", href_ "./css/ripley.css"]
               <> link_ [rel_ "apple-touch-icon"
                        , sizes_ "180x180"
                        , href_ "./apple-touch-icon.png"]
               <> link_ [rel_ "icon"
                        , type_ "image/png"
                        , sizes_ "32x32"
                        , href_ "./favicon-32x32.png"]
               <> link_ [rel_ "icon"
                        , type_ "image/png"
                        , sizes_ "16x16"
                        , href_ "./favicon-16x16.png"]
               <> link_ [rel_ "manifest"
                        , href_ "./site.webmanifest"]



pageHeader :: Html ()
pageHeader =
  nav_ [class_ "navbar navbar-inverse"] $
    div_ [class_ "container-fluid"] $ do
      div_ [class_ "navbar-header"] $ (do
        button_ [ type_ "button"
                , class_ "navbar-toggle collapsed"
                , (term "data-toggle") "collapse"
                , (term "data-target") "#dr-headmenu"
                , (term "aria-expanded") "false"
                ]
          (span_ [class_ "sr-only"] "Toggle navigation" <> mconcat (replicate 3 (span_ [class_ "icon-bar"] "")))
        a_ [class_ "navbar-brand indexlink", href_ "./index.html"] "David Ripley")
      div_ [class_ "collapse navbar-collapse", id_ "dr-headmenu"]
        (ul_ [class_ "nav navbar-nav"]
                   ((li_ [class_ "writinglink"] (a_ [href_ "./writing.html"] "Writing"))
                 <> (li_ [class_ "presentationlink"] (a_ [href_ "./presentations.html"] "Presentations"))  
                 <> (li_ [class_ "cvlink"] (a_ [href_ "./ripleyCV.pdf", target_ "_blank"] "CV"))))

pageFooter :: Html ()
pageFooter =
    container_ $
      row_ $ do
        div_ [class_ "col-md-2"] mempty
        div_ [class_ "col-md-8 footer"] $ do
          div_ [class_ "col-md-6"]
            (table_ $ do
              (tr_ $ do
                (td_ [class_ "contact-icon"] (span_ [class_ "fa fa-fw fa-inbox"] ""))
                (td_ (emailLink "davewripley@gmail.com"))))
          div_ [class_ "col-md-6"]
            (table_ $ do
              (tr_ $ do
                (td_ [class_ "contact-icon"] (span_ [class_ "fa fa-fw fa-envelope"] ""))
                (td_ (monashPhilLink "Philosophy Department")))
              (tr_ $ do
                (td_ [class_ "contact-icon"] "")
                (td_ (monashSchoolLink "SOPHIS")))
              (tr_ $ do
                (td_ [class_ "contact-icon"] "")
                (td_ (p_ [class_ "address"] "Building 11")))
              (tr_ $ do
                (td_ [class_ "contact-icon"] "")
                (td_ (p_ [class_ "address"] "Monash University, VIC 3800")))
              (tr_ $ do
                (td_ [class_ "contact-icon"] "")
                (td_ (p_ [class_ "address"] "Australia"))))
        div_ [class_ "col-md-2"] mempty
                 

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
        <> monashPhilLink "philosophy department"
        <> " at Monash University.")
  <> p_ "My research focuses on languages, logics, and the relations between them."
  <> p_ ("I'm also a member of the "
        <> aalLink "Australasian Association for Logic.")
  <> p_ ("You can email me at "
        <> emailLink "davewripley@gmail.com"
        <> ".")


--SECTION: presentation page

presentationPage :: [Presentation] -> Html ()
presentationPage pres = pageFrom (presentationBody pres) (navbarJS "presentationlink")


presentationAuthors :: AuthorCat -> Html ()
presentationAuthors Solo = mempty
presentationAuthors CERvR = presentationAuthors (Other [ "pabloCobreros", "paulEgre", "davidRipley", "robertVanRooij" ])
presentationAuthors (Other as) = p_ [class_ "presentation-authors" ] (mconcat $ intersperse ", " (map makeAuthorLink as))


presRow :: Presentation -> Html ()
presRow p =
  row_ [class_ "presentation-row"] $ do
    div_ [class_ "col-md-10 pres-bubble"] $ do
        div_ [class_ "col-md-5"]
            ((p_ [class_ "talktitle"] (toHtml $ presTitle p))
             <> presentationAuthors (presAuthors p))
        div_ [class_ "col-md-7"]
             (ul_ [class_ "presentation-venue"]
             (listItems [class_ "presentation-venue"] (map toHtml $ presLocations p)))
    extrasMarks p


presentationBody :: [Presentation] -> Html ()
presentationBody pres = do
  topLabel "Presentations"
  container_ $ do
    div_ [class_ "mainbits"] $ do
        pileUp (map presRow pres)
        


--SECTION: writing page

searchJS :: Html ()
searchJS = script_ [src_ "./js/search.js"] ""

writingPage :: [WritingPiece] -> Html ()
writingPage ps = pageFrom (writingBody ps) (navbarJS "writinglink" <> searchJS)

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

writingBody :: [WritingPiece] -> Html ()
writingBody ps = do
    topLabel "Writing"
    container_ [class_ "mainbits"] $ 
      row_ $ do
        div_ [class_ "col-md-3 searchbar"]
            (searchBar <> searchSort <> searchFilters <> philpapersBit)
        div_ [class_ "col-md-9 searchresults"]
            (ul_ [class_ "writingdisplay"] (pileUp $ map makeEntry (zip (sortBy pieceSort ps) [1..])))

paperTitleHead :: WritingPiece -> Html ()
paperTitleHead p =
  case (writingUrl p) of
    "" -> pt
    u  -> a_ [ href_ u
             , class_ "title-link"
             , target_ "_blank"
             ] pt
  where pt = toHtml (title p)

makeEntry :: (WritingPiece, Int) -> Html ()
makeEntry (p, n) = 
  let cls = "paperbubble " <> (classify $ authorCat p)
      auths = map makeAuthorLink (wpAuthorTags p)
      ci = "citation" <> (T.pack $ show n)
      ai = "abstract" <> (T.pack $ show n)
      bi = "bibtex" <> (T.pack $ show n)
      ali1 = case abstract p of
               Nothing -> mempty
               Just _  ->
                 li_ [term "role" "presentation"]
                   (a_ [href_ ("#" <> ai), term "aria-controls" "abstract", term "role" "tab", term "data-toggle" "pill"] "Abstract")
      ali2 = case abstract p of
               Nothing -> mempty
               Just ab ->
                 div_ [term "role" "tabpanel", class_ "tab-pane", id_ ai]
                   (p_ [class_ "abstract"] (toHtml ab))
  in li_ [class_ cls] $ do
         p_ [class_ "ptitle"] (paperTitleHead p)
         p_ [class_ "pauthors"] (mconcat $ intersperse ", " auths)
         div_ [class_ "row"]
           (div_ [class_ "col-sm-11 paperinfo"] $ do
             (div_ [class_ "col-sm-10"]
               (div_ [class_ "tab-content"] $ do
                 div_ [term "role" "tabpanel", class_ "tab-pane active", id_ ci] (p_ [class_ "pvenue"] (wpVenue p))
                 ali2
                 div_ [term "role" "tabpanel", class_ "tab-pane", id_ bi] (p_ [class_ "bibtex"] (pre_ [class_ "bibtex"] (toHtml $ wpBibtex p)))))
             (div_ [class_ "col-sm-2"]
               (ul_ [class_ "nav nav-pills", term "role" "tablist"] $ do
                 li_ [term "role" "presentation", class_ "active"]
                   (a_ [href_ ("#" <> ci), term "aria-controls" "citation", term "role" "tab", term "data-toggle" "pill"] "Citation")
                 ali1
                 li_ [term "role" "presentation"]
                   (a_ [href_ ("#" <> bi), term "aria-controls" "bibtex", term "role" "tab", term "data-toggle" "pill"] "BibTeX"))))


pieceSort :: WritingPiece -> WritingPiece -> Ordering
pieceSort p1 p2 =
  case (year p1, year p2) of
    (Nothing, Nothing) -> nameSort p1 p2
    (Nothing, _)       -> LT
    (_      , Nothing) -> GT
    (Just y1, Just z1) -> case z1 `compare` y1 of
                            EQ -> nameSort p1 p2
                            x  -> x
  where
    nameSort p1 p2 = (title p1) `compare` (title p2)



  
--SECTION: valuations page

valuationsPage :: Html ()
valuationsPage = pageFrom valuationsBody (navbarJS "indexlink")

valuationsBody :: Html ()
valuationsBody =
  div_ [id_ "valuations-app"] $ do
      script_ [src_ "./js/valuations.js"] ""
      script_ [] "var app = Elm.Main.init({node: document.getElementById(\"valuations-app\")});"
    


--SECTION: generation

dirPrefix :: FilePath
dirPrefix = "./for-upload/"

websiteMain :: IO ()
websiteMain = do
  System.Directory.createDirectoryIfMissing True dirPrefix
  Data.Text.Lazy.IO.writeFile (dirPrefix <> "index.html") (renderText indexPage)
  mpres <- presentations
  Data.Text.Lazy.IO.writeFile (dirPrefix <> "presentations.html") (renderText . presentationPage $ maybe [] id mpres)
  mpieces <- pieces
  Data.Text.Lazy.IO.writeFile (dirPrefix <> "writing.html") (renderText . writingPage $ maybe [] id mpieces)
  Data.Text.Lazy.IO.writeFile (dirPrefix <> "valuations.html") (renderText valuationsPage)