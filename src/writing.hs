{-# LANGUAGE OverloadedStrings #-}

module Writing (writing, Piece, pieceTitle, pieceAuthorTags, pieceUrl, pieceVenue, pieceYear, pieceAuthorCat) where

import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Text (Text)
import Data.ByteString.Lazy (readFile, ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Lucid
import Data.Monoid ((<>), mempty, mconcat)
import Data.List (intersperse)

import Authors (Author, authors)
import WebsiteTools (AuthorCat(..), classify, doiToLink, sHtml)



data ArticlePublicationData =
  ForthcomingAPD  
  | APD { yearA  :: Int
        , volumeA :: Int
        , numberA :: Maybe Int
        , startpageA :: Int
        , endpageA   :: Int
        , doiLinkA   :: Html ()
        } deriving (Show)

data Article = Article { titleA   :: Text
                       , journalA  :: Text
                       , authorcatA :: AuthorCat
                       , urlA :: Text
                       , pdA  :: ArticlePublicationData
                       } deriving (Show)

data ChapterPublicationData =
  ForthcomingCPD
  | CPD { yearC :: Int
        , startpageC :: Int
        , endpageC :: Int
        , booklinkC :: Html ()
        } deriving (Show)

                              
data Chapter = Chapter { titleC :: Text
                       , authorcatC :: AuthorCat
                       , booktitleC :: Text
                       , editorC :: [Text]
                       , publisherC :: Text
                       , urlC :: Text
                       , pdC :: ChapterPublicationData
                       } deriving (Show)

data Piece = A Article | C Chapter deriving (Show)



--Accessors:

pieceTitle :: Piece -> Text
pieceTitle (A a) = titleA a
pieceTitle (C c) = titleC c

pieceAuthorCat :: Piece -> AuthorCat
pieceAuthorCat (A a) = authorcatA a
pieceAuthorCat (C c) = authorcatC c

getAuth :: Text -> Author
getAuth tg = fromJust (M.lookup tg authors)

pieceAuthorTags :: Piece -> [Text]
pieceAuthorTags p = case ac of
                      Solo     -> ["davidRipley"]
                      CERvR    -> [ "pabloCobreros"
                                  , "paulEgre"
                                  , "davidRipley"
                                  , "robertVanRooij"
                                  ]
                      Other as -> as
                    where
                      ac = case p of
                          (A art) -> authorcatA art
                          (C chp) -> authorcatC chp

pieceUrl :: Piece -> Text
pieceUrl (A a) = urlA a
pieceUrl (C c) = urlC c

pieceYear :: Piece -> Maybe Int
pieceYear (A a) = case pdA a of
                    ForthcomingAPD -> Nothing
                    ap -> Just (yearA ap)
pieceYear (C c) = case pdC c of
                    ForthcomingCPD -> Nothing
                    cp -> Just (yearC cp)


pieceVenue :: Piece -> Html ()
pieceVenue (A art) = (i_ (toHtml $ journalA art)) <> ", " <> pubData
  where
    pubData = case pdA art of
                ForthcomingAPD -> "forthcoming."
                apd -> (sHtml $ volumeA apd)
                           <> num
                           <> ":"
                           <> (sHtml $ startpageA apd)
                           <> "-"
                           <> (sHtml $ endpageA apd)
                           <> ", "
                           <> (sHtml $ yearA apd)
                           <> "."
                           where num = case numberA apd of
                                         Nothing -> mempty
                                         Just n  -> "(" <> sHtml n <> ")"
pieceVenue (C chp) = "In "
                     <> (i_ (toHtml $ booktitleC chp))
                     <> ", ed "
                     <> (toHtml $ mconcat (intersperse ", " $ editorC chp))
                     <> ". "
                     <> pubData
  where
    pubData = case pdC chp of
                ForthcomingCPD -> "Forthcoming."
                cpd -> "Pages "
                       <> (sHtml $ startpageC cpd)
                       <> "-"
                       <> (sHtml $ endpageC cpd)
                       <> ", "
                       <> (sHtml $ yearC cpd)
                       <> "."



-- Data


writing :: [Piece]
writing = [ A (Article
              "Blurring: an approach to conflation"
              "Notre Dame Journal of Formal Logic"
              Solo
              "./papers/blurring.pdf"
              ForthcomingAPD)
          , A (Article
              "How mathematics can make a difference"
              "Philosophers' Imprint"
              (Other [ "samBaron"
                     , "markColyvan"
                     , "davidRipley"
                     ])
              ""
              ForthcomingAPD)
          , A (Article
              "Naive set theory and nontransitive logic"
              "Review of Symbolic Logic"
              Solo
              "./papers/nstntl.pdf"
              (APD 2015 8 (Just 3) 553 571 ""))
          , A (Article
              "Contractions of noncontractive consequence relations"
              "Review of Symbolic Logic"
              (Other ["rohanFrench", "davidRipley"])
              "./papers/cncr.pdf"
              (APD 2015 8 (Just 3) 506 528 ""))
          , A (Article
              "Comparing substructural theories of truth"
              "Ergo"
              Solo
              "http://dx.doi.org/10.3998/ergo.12405314.0002.013"
              (APD 2015 2 (Just 13) 299 328 "10.3998/ergo.12405314.0002.013"))
          , A (Article
              "Contraction and closure"
              "Thought"
              Solo
              "./papers/cc.pdf"
              (APD 2015 4 (Just 2) 131 138 "10.1002/tht3.166"))
          , A (Article
              "Tolerating gluts"
              "Mind"
              (Other [ "zachWeber"
                     , "davidRipley"
                     , "grahamPriest"
                     , "dominicHyde"
                     , "markColyvan"
                     ])
               "http://mind.oxfordjournals.org/cgi/reprint/fzu057?ijkey=nQCUytimlfdBzuz&keytype=ref"
               (APD 2014 123 (Just 491) 813 828 mempty))
          , C (Chapter
              "Bilateralism, coherence, warrant"
              Solo
              "Act-Based Conceptions of Propositional Content"
              ["Friederike Moltmann", "Mark Textor"]
              "Oxford University Press"
              "./papers/bcw.pdf"
              ForthcomingCPD)
          , C (Chapter
              "Priest's motorbike and tolerant identity"
              CERvR
              "Recent Trends in Philosophical Logic"
              [ "Roberto Ciuni"
              , "Heinrich Wansing"
              , "Caroline Wilkommen"
              ]
              "Springer"
              "./papers/pmti.pdf"
              (CPD 2014 75 85 ""))
          , A (Article
              "Anything goes"
              "Topoi"
              Solo
              "./papers/ag.pdf"
              (APD 2015 34 (Just 1) 25 36 "10.1007/s11245-014-9261-8"))
          , A (Article
              "Pragmatic interpretations of vague expressions"
              "Journal of Philosophical Logic"
              CERvR
              "./papers/pive.pdf"
              (APD 2015 44 (Just 4) 375 393 "10.1007/s10992-014-9325-7"))
          , A (Article
              "Paraconsistent logic"
              "Journal of Philosophical Logic"
              Solo
              "./papers/pl.pdf"
              ForthcomingAPD)
          , C (Chapter
              "Experimental philosophical logic"
              Solo
              "A Companion to Experimental Philosophy"
              [ "Justin Sytsma"
              , "Wesley Buckwalter"
              ]
              "Wiley"
              "./papers/xpl.pdf"
              ForthcomingCPD)
          , C (Chapter
              "Vagueness, truth, and permissive consequence"
              CERvR
              "Unifying the Philosophy of Truth"
              [ "Theodora Achourioti"
              , "Henri Galinon"
              , "José Martínez Fernández"
              , "Kentaro Fujimoto"
              ]
              "Springer"
              "./papers/vtpc.pdf"
              (CPD 2015 409 430 "http://www.springer.com/us/book/9789401796729"))
          , C (Chapter
              "Nonclassical theories of truth"
              (Other [ "jcBeall", "davidRipley" ])
              "the Oxford Handbook of Truth"
              [ "Michael Glanzberg" ]
              "Oxford University Press"
              ""
              ForthcomingCPD)
          , C (Chapter
              "Embedding denial"
              Solo
              "Foundations of Logical Consequence"
              [ "Colin Caret"
              , "Ole Hjortland"
              ]
              "Oxford University Press"
              "./papers/ed.pdf"
              (CPD 2015 289 309 "http://global.oup.com/academic/product/foundations-of-logical-consequence-9780198715696"))
          , A (Article
              "Vagueness and order effects in color categorization"
              "Journal of Logic, Language, and Information"
              (Other [ "paulEgre", "vincentDeGardelle", "davidRipley" ])
              "./papers/voe.pdf"
              (APD 2013 22 (Just 4) 391 420 "10.1007/s10849-013-9183-7"))
          , A (Article
              "Reaching transparent truth"
              "Mind"
              CERvR
              "http://mind.oxfordjournals.org/cgi/reprint/fzt110?ijkey=xGKyG7colWYSJzh&keytype=ref"
              (APD 2013 122 (Just 488) 841 866 "10.1093/mind/fzt110"))
          , A (Article
              "Identity, Leibniz's law, and nontransitive reasoning"
              "Metaphysica"
              CERvR
              "./papers/ill.pdf"
              (APD 2013 14 (Just 2) 253 264 "10.1007/s12133-013-0125-2"))
          , A (Article
              "Revising up"
              "Philosophers' Imprint"
              Solo
              "http://quod.lib.umich.edu/cgi/p/pod/dod-idx/revising-up-strengthening-classical-logic-in-the-face.pdf?c=phimp;idno=3521354.0013.005"
              (APD 2013 13 (Just 5) 1 13 ""))
          , A (Article
              "Paradoxes and failures of cut"
              "Australasian Journal of Philosophy"
              Solo
              "./papers/pafc.pdf"
              (APD 2013 91 (Just 1) 139 164 ""))
          , C (Chapter
              "Sorting out the sorites"
              Solo
              "Paraconsistency: Logic and Applications"
              [ "Francesco Berto"
              , "Edwin Mares"
              , "Koji Tanaka"
              ]
              "Springer"
              "./papers/sos.pdf"
              (CPD 2013 329 348 mempty))
          , A (Article
              "Explaining the abstract/concrete paradoxes in moral psychology"
              "Review of Philosophy and Psychology"
              (Other [ "ericMandelbaum", "davidRipley" ])
              "./papers/nbar.pdf"
              (APD 2012 3 (Just 3) 351 368 ""))
          , A (Article
              "Structures and circumstances"
              "Synthese"
              Solo
              "./papers/sc.pdf"
              (APD 2012 189 (Just 1) 97 118 ""))
          , A (Article
              "Tolerance and mixed consequence in the s'valuationist setting"
              "Studia Logica"
              CERvR
              "./papers/tmcsv.pdf"
              (APD 2012 100 (Just 4) 855 877 ""))
          , A (Article
              "Conservatively extending classical logic with transparent truth"
              "Review of Symbolic Logic"
              Solo
              "./papers/cecltt.pdf"
              (APD 2012 5 (Just 2) 354 378 ""))
          , A (Article
              "On the ternary relation and conditionality"
              "Journal of Philosophical Logic"
              (Other [ "jcBeall"
                     , "rossBrady"
                     , "michaelDunn"
                     , "allenHazen"
                     , "edwinMares"
                     , "robertMeyer"
                     , "grahamPriest"
                     , "gregRestall"
                     , "davidRipley"
                     , "johnSlaney"
                     , "richardSylvan"
                     ])
              "./papers/ternary.pdf"
              (APD 2012 41 (Just 3) 595 612 ""))
          , A (Article
              "Tolerant, classical, strict"
              "Journal of Philosophical Logic"
              CERvR
              "http://link.springer.com/content/pdf/10.1007%2Fs10992-010-9165-z.pdf"
              (APD 2012 41 (Just 2) 347 385 ""))
          , C (Chapter
              "Inconstancy and inconsistency"
              Solo
              "Understanding Vagueness"
              [ "Petr Cintula"
              , "Christian G. Fermüller"
              , "Lluís Godo"
              , "Petr Hájek"
              ]
              "College Publications"
              "./papers/ii.pdf"
              (CPD 2011 41 58 ""))
          , A (Article
              "Negation, denial, and rejection"
              "Philosophy Compass"
              Solo
              "./papers/ndr.pdf"
              (APD 2011 6 (Just 9) 622 629 ""))
          , C (Chapter
              "Contradictions at the borders"
              Solo
              "Vagueness in Communication"
              [ "Rick Nouwen"
              , "Robert van Rooij"
              , "Uli Sauerland"
              , "Hans-Christian Schmitz"
              ]
              "Springer"
              "./papers/catb.pdf"
              (CPD 2011 169 188 ""))
          , A (Article
              "Responsibility and the brain sciences"
              "Ethical Theory and Moral Practice"
              (Other [ "felipeDeBrigard", "ericMandelbaum", "davidRipley" ])
              "./papers/rbs.pdf"
              (APD 2009 12 (Just 5) 511 524 ""))
          , A (Article
              "Analetheism and dialetheism"
              "Analysis"
              (Other [ "jcBeall", "davidRipley" ])
              "./papers/ad.pdf"
              (APD 2004 64 (Just 1) 30 35 ""))
          ]