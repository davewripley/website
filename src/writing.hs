{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module Writing (papers, Paper(..), paperAuthorTags, paperVenue, paperYear, paperBibtex) where

import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Lazy (readFile, ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Lucid
import Data.Monoid ((<>), mempty, mconcat)
import Data.List (intersperse)

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Yaml.Aeson as Y

import qualified Data.ByteString as BS

import Authors (Author(..), authors)
import WebsiteTools (AuthorCat(..), classify, doiToLink, sHtml)



data ArticlePublicationData =
  ForthcomingAPD  
  | APD { yearA  :: Int
        , volumeA :: Int
        , numberA :: Maybe Int
        , startpageA :: Int
        , endpageA   :: Int
        , doiLinkA   :: Text
        } deriving (Show)

data Article = Article { titleA   :: Text
                       , journalA  :: Text
                       , authorcatA :: AuthorCat
                       , urlA :: Text
                       , pdA  :: ArticlePublicationData
                       , bibtagA :: Text
                       , abstractA :: Maybe Text
                       } deriving (Show)

data ChapterPublicationData =
  ForthcomingCPD
  | CPD { yearC :: Int
        , startpageC :: Int
        , endpageC :: Int
        , booklinkC :: Text
        } deriving (Show)

                              
data Chapter = Chapter { titleC :: Text
                       , authorcatC :: AuthorCat
                       , booktitleC :: Text
                       , editorC :: [Text]
                       , publisherC :: Text
                       , urlC :: Text
                       , pdC :: ChapterPublicationData
                       , bibtagC :: Text
                       , abstractC :: Maybe Text
                       } deriving (Show)

data Piece = A Article | C Chapter deriving (Show)


data AorC =
    Ar { journal :: Text
       , volume :: Maybe Int
       , number :: Maybe Int
       , doiLink :: Maybe Text
       }
  | Ch { booktitle :: Text
       , editor :: [Text]
       , publisher :: Text
       } deriving (Show, Eq)

data PaperPubData = Published { year :: Int
                              , startPage :: Int
                              , endPage :: Int
                              } deriving (Show, Eq)
              
data Paper = Paper { title :: Text
                   , authorCat :: AuthorCat
                   , paperUrl :: Text
                   , bibtag :: Text
                   , abstract :: Maybe Text
                   , pubData :: Maybe PaperPubData
                   , aorc :: AorC
                   } deriving (Show, Eq)

deriveJSON defaultOptions{sumEncoding = TwoElemArray} ''AorC
deriveJSON defaultOptions ''PaperPubData
deriveJSON defaultOptions ''Paper

--Accessors:

getAuth :: Text -> Author
getAuth tg = fromJust (M.lookup tg authors)

paperAuthorTags :: Paper -> [Text]
paperAuthorTags p = case authorCat p of
                      Solo     -> ["davidRipley"]
                      CERvR    -> [ "pabloCobreros"
                                  , "paulEgre"
                                  , "davidRipley"
                                  , "robertVanRooij"
                                  ]
                      Other as -> as

paperYear :: Paper -> Maybe Int
paperYear = (fmap year) . pubData

paperVenue :: Paper -> Html ()
paperVenue p = case (aorc p) of
  a@(Ar{..}) -> (i_ (toHtml $ journal)) <> ", " <> t
    where
      t = case pubData p of
        Nothing -> "forthcoming."
        (Just pd) -> vol <> num <> ":"
                     <> (sHtml $ startPage pd)
                     <> "-"
                     <> (sHtml $ endPage pd)
                     <> ", "
                     <> (sHtml $ year pd)
                     <> "."
                     where
                       vol = case volume of
                               Nothing -> mempty
                               Just v  -> sHtml v
                       num = case number of
                               Nothing -> mempty
                               Just n  -> "(" <> sHtml n <> ")"
  c@(Ch{..}) -> "In " <> (i_ (toHtml $ booktitle))
                      <> ", ed "
                      <> (toHtml $ mconcat (intersperse ", " $ editor))
                      <> ". "
                      <> t
    where
      t = case pubData p of
        Nothing -> "Forthcoming."
        (Just pd) -> "Pages "
                     <> (sHtml $ startPage pd)
                     <> "-"
                     <> (sHtml $ endPage pd)
                     <> ", "
                     <> (sHtml $ year pd)
                     <> "."

authname :: Text -> Maybe Text
authname t = name <$> M.lookup t authors

getText :: Maybe Text -> Text
getText m =
  case m of
    Just t -> t
    Nothing -> mempty

bibTeXauths :: Paper -> Text
bibTeXauths = btChars . T.intercalate " and " . map (getText . authname) . paperAuthorTags

btChars :: Text -> Text
btChars = T.concatMap cleanup
  where
    cleanup c =
      case c of
        '\225' -> "{\\'{a}}"
        '\233' -> "{\\'{e}}"
        '\237' -> "{\\'{i}}"
        '\252' -> "{\\\"{u}}"
        _      -> T.singleton c

write :: Show a => a -> Text
write = T.pack . show

paperBibtex :: Paper -> Text
paperBibtex p = case aorc p of
  a@(Ar{..}) -> T.concat $
    [ "@article{"
    , bibtag p
    , ",\n   author = {"
    , bibTeXauths p
    , "},\n   title = {"
    , title p
    , "},\n   journal = {"
    , journal
    , "},\n   "
    ] ++ rest ++
    [ "}\n" ]
    where
      rest = case pubData p of
        Nothing -> [ "note = {Forthcoming}\n" ]
        (Just pd) ->
          [ "year = {"
          , write (year pd)
          , "},\n   volume = {"
          , vol
          , "},\n   number = {"
          , nmb
          , "},\n   pages = {"
          , (write $ startPage pd) <> "--" <> (write $ endPage pd)
          , "}\n"
          ]
      vol = case volume of
              Nothing -> mempty
              Just v  -> write v
      nmb = case number of
              Nothing -> mempty
              Just n  -> write n
  c@(Ch{..}) -> T.concat $
    [ "@incollection{"
    , bibtag p
    , ",\n   author = {"
    , bibTeXauths p
    , "},\n   title = {"
    , title p
    , "},\n   booktitle = {"
    , booktitle
    , "},\n   editor = {"
    , btChars . T.intercalate " and " $ editor
    , "},\n   publisher = {"
    , publisher
    , "},\n   "
    ] ++ rest ++
    [ "}\n" ]
    where
      rest = case pubData p of
        Nothing -> [ "note = {Forthcoming}\n" ]
        (Just pd) ->
          [ "year = {"
          , write (year pd)
          , "},\n   pages = {"
          , (write $ startPage pd) <> "--" <> (write $ endPage pd)
          , "}\n"
          ]

pdFromPiece :: Piece -> Maybe PaperPubData
pdFromPiece (A art) = case pdA art of
  ForthcomingAPD -> Nothing
  APD{..} -> Just $ Published { year = yearA
                       , startPage = startpageA
                       , endPage = endpageA
                       }
pdFromPiece (C cha) = case pdC cha of
  ForthcomingCPD -> Nothing
  CPD{..} -> Just $ Published { year = yearC
                       , startPage = startpageC
                       , endPage = endpageC
                       }
                       

aorcFromPiece :: Piece -> AorC
aorcFromPiece (A art) = Ar { journal = journalA art
                           , volume = vol
                           , number = num
                           , doiLink = doi
                           }
  where
    vol = case pdA art of
            ForthcomingAPD -> Nothing
            APD{..}        -> Just volumeA
    num = case pdA art of
            ForthcomingAPD -> Nothing
            APD{..}        -> numberA
    doi = case pdA art of
            ForthcomingAPD -> Nothing
            APD{..}        -> Just doiLinkA
aorcFromPiece (C cha) = Ch { booktitle = booktitleC cha
                           , editor = editorC cha
                           , publisher = publisherC cha
                           }

paperFile :: FilePath
paperFile = "./src/papers.yaml"

papers :: IO (Maybe [Paper])
papers = do
  pData <- BS.readFile paperFile
  return (Y.decode pData)




pieceToPaper :: Piece -> Paper
pieceToPaper p@(A art) =
  Paper { title = titleA art
        , authorCat = authorcatA art
        , paperUrl = urlA art
        , bibtag = bibtagA art
        , abstract = abstractA art
        , pubData = pdFromPiece p
        , aorc = aorcFromPiece p
        }
pieceToPaper p@(C cha) =
  Paper { title = titleC cha
        , authorCat = authorcatC cha
        , paperUrl = urlC cha
        , bibtag = bibtagC cha
        , abstract = abstractC cha
        , pubData = pdFromPiece p
        , aorc = aorcFromPiece p
        }
        

-- Data

{-
writing :: [Piece]
writing = [ A (Article
              "On the 'transitivity' of consequence relations"
              "Journal of Logic and Computation"
              Solo
              "./papers/otocr.pdf"
              ForthcomingAPD
              "ripley:otocr"
              (Just "The relations logicians tend to think of as consequence relations are almost never transitive, at least not in the usual relation-theoretic sense of 'transitive'. Yet it is common to hear them described as 'transitive', and to see rules impose to ensure 'transitivity' of these relations. This paper attempts to clarify the situation."))
          , C (Chapter
              "Comparing some substructural strategies dealing with vagueness"
              CERvR
              "Information Processing and Management of Uncertainty in Knowlede-Based Systems, Part II: Proceedings of IPMU 2016"
              [ "Joao Paolo Carvalho"
              , "Marie-Jeanne Lesot"
              , "Uzay Kaymak"
              , "Susana Vieira"
              , "Bernadette Bouchon-Meunier"
              , "Ronald R. Yager"
              ]
              "Springer"
              "./papers/cssv.pdf"
              (CPD 2016 161 172 "www.springer.com/us/book/9783319405803")
              "cervr:cssv"
              (Just "We compare some nonmonotonic and nontransitive logical approaches to vague predicates, exploring ways to build nonmonotonic logics sensitive to at least some pragmatic constraints on top of our earlier work on nontransitive logics."))
          , A (Article
              "Vagueness is a kind of conflation"
              "Logic and Logical Philosophy"
              Solo
              "http://apcz.pl/czasopisma/index.php/LLP/article/view/LLP.2016.020"
              (APD 2017 26 (Just 1) 115 135 "")
              "ripley:vkc"
              (Just "This paper sketches an understanding of conflation and vagueness according to which the latter is a special kind of the former. First, I sketch a particular understanding of conflation. Then, I go on to argue that vague concepts fit directly into this understanding. This picture of vagueness is related, but not identical, to a number of existing accounts."))
          , C (Chapter
              "'Transitivity' of consequence relations"
              Solo
              "Logic, Rationality, and Interaction: Proceedings of LORI V"
              [ "Wiebe van der Hoek"
              , "Wesley Holliday"
              , "Wen-Fang Wang"
              ]
              "Springer"
              "./papers/tocr.pdf"
              (CPD 2015 328 340 "www.springer.com/us/book/9783662485606")
              "ripley:tocr"
              (Just "The relations logicians tend to think of as consequence relations are almost never transitive, at least not in the usual relation-theoretic sense of 'transitive'. Yet it is common to hear them described as 'transitive', and to see rules impose to ensure 'transitivity' of these relations. This paper attempts to clarify the situation."))
          , A (Article
              "Blurring: an approach to conflation"
              "Notre Dame Journal of Formal Logic"
              Solo
              "./papers/blurring.pdf"
              ForthcomingAPD
              "ripley:blurring"
              (Just "I consider the phenomenon of conflation---treating distinct things as one---and develop logical tools for modeling it. These tools involve a purely consequence-theoretic treatment,  independent of any proof or model theory, as well as a four-valued valuational treatment."))
          , A (Article
              "How mathematics can make a difference"
              "Philosophers' Imprint"
              (Other [ "samBaron"
                     , "markColyvan"
                     , "davidRipley"
                     ])
              "./papers/hmmd.pdf"
              ForthcomingAPD
              "bcr:hmmd"
              Nothing)
          , A (Article
              "Naive set theory and nontransitive logic"
              "Review of Symbolic Logic"
              Solo
              "./papers/nstntl.pdf"
              (APD 2015 8 (Just 3) 553 571 "")
              "ripley:nstntl"
              (Just "In a recent series of papers, I and others have advanced new logical approaches to familiar paradoxes. The key to these approaches is to accept full classical logic, and to accept the principles that cause paradox, while preventing trouble by allowing a certain sort of nontransitivity. Earlier papers have treated paradoxes of truth and vagueness. The present paper begins to extend the approach to deal with the familiar paradoxes arising in naive set theory, pointing out some of the promises and pitfalls of such an approach."))
          , A (Article
              "Contractions of noncontractive consequence relations"
              "Review of Symbolic Logic"
              (Other ["rohanFrench", "davidRipley"])
              "./papers/cncr.pdf"
              (APD 2015 8 (Just 3) 506 528 "")
              "fr:cncr"
              (Just "Some theorists have developed formal approaches to truth that depend on counterexamples to the structural rules of contraction. Here, we study such approaches, with an eye to helping them respond to a certain kind of objection. We define a contractive relative of each noncontractive relation, for use in responding to the objection in question, and we explore one example: the contractive relative of multiplicative-additive affine logic with transparent truth, or MAALT."))
          , A (Article
              "Comparing substructural theories of truth"
              "Ergo"
              Solo
              "http://dx.doi.org/10.3998/ergo.12405314.0002.013"
              (APD 2015 2 (Just 13) 299 328 "10.3998/ergo.12405314.0002.013")
              "ripley:cstt"
              (Just "Substructural theories of truth are theories based on logics that do not include the full complement of usual structural rules. Existing substructural approaches fall into two main families: noncontractive approaches and nontransitive approaches. This paper provides a sketch of these families, and argues for two claims: first, that substructural theories are better-positioned than other theories to grapple with the truth-theoretic paradoxes, and second---more tentatively---that nontransitive approaches are in turn better-positioned than noncontractive approaches."))
          , A (Article
              "Contraction and closure"
              "Thought"
              Solo
              "./papers/cc.pdf"
              (APD 2015 4 (Just 2) 131 138 "10.1002/tht3.166")
              "ripley:cc"
              (Just "In this paper, I consider the connection between consequence relations and closure operations. I argue that one familiar connection makes good sense of some usual applications of consequence relations, and that a largeish family of familiar noncontractive consequence relations cannot respect this familiar connection."))
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
               (APD 2014 123 (Just 491) 813 828 mempty)
               "wrphc:tg"
               (Just "In  an  approach  to  vagueness  using  the  paraconsistent logic LP,  borderline cases of vague predicates are contradictory---logical gluts. In ‘Finding Tolerance without Gluts’, Jc Beall argues against such an account of vagueness. He constructs an alternative theory, and argues that ‘[t]he result enjoys all the virtues of  the LP solution but without the gluts’. He concludes that his alternative is therefore preferable to the LP solution. In what follows, we will demonstrate that this is not the case: Beall’s account does not do all the things that a paraconsistent solution can do. In fact, it is the other way around: the paraconsistent account can do everything that Beall’s theory can do, and more. And some of the ‘more’ is very important. We will demonstrate this by discussing each of the three objections to his own project which Beall raises and rejects, arguing that his replies fail in each case. This note is not solely a reply to Beall. Several quite new points emerge  in  the  discussion, clarifying the paraconsistent account."))
          , C (Chapter
              "Bilateralism, coherence, warrant"
              Solo
              "Act-Based Conceptions of Propositional Content"
              ["Friederike Moltmann", "Mark Textor"]
              "Oxford University Press"
              "./papers/bcw.pdf"
              ForthcomingCPD
              "ripley:bcw"
              Nothing)
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
              (CPD 2014 75 85 "")
              "cervr:pmti"
              Nothing)
          , A (Article
              "Anything goes"
              "Topoi"
              Solo
              "./papers/ag.pdf"
              (APD 2015 34 (Just 1) 25 36 "10.1007/s11245-014-9261-8")
              "ripley:ag"
              (Just "What sorts of sequent-calculus rules succeed in specifying a legitimate piece of vocabulary? Following on Arthur Prior’s discussion of the connective tonk, there have been a flurry of criteria  offered. Here, I step back a bit, examining  the  role  of  structural  rules in an inferentialist theory of meaning, and sketch a theory on which any way at all of giving left  and right sequent rules  for  a piece  of vocabulary is ok. Tonk, among other things, is a full citizen of coherent-idea-land."))
          , A (Article
              "Pragmatic interpretations of vague expressions"
              "Journal of Philosophical Logic"
              CERvR
              "./papers/pive.pdf"
              (APD 2015 44 (Just 4) 375 393 "10.1007/s10992-014-9325-7")
              "cervr:pive"
              (Just "Recent experiments have shown that naive speakers find borderline contradictions involving vague predicates acceptable. In \"Tolerant, classical, strict\", we proposed a pragmatic explanation of the acceptability of borderline contradictions, building on a three-valued semantics. In a reply, Alxatib et al. show, however,  that  the  pragmatic  account  predicts  the  wrong  interpretations  for  some examples involving disjunction, and propose as a remedy a semantic analysis instead, based on fuzzy logic. In this paper we provide an explicit global pragmatic interpretation rule, based on a somewhat richer semantics, and show that with its help the problem can be overcome in pragmatics after all. Furthermore, we use this pragmatic interpretation rule to define a new (nonmonotonic) consequence relation and discuss some of its properties."))
          , A (Article
              "Paraconsistent logic"
              "Journal of Philosophical Logic"
              Solo
              "./papers/pl.pdf"
              (APD 2015 44 (Just 6) 771 780 "10.1007/s10992-015-9358-6")
              "ripley:pl"
              (Just "In  some  logics, anything  whatsoever  follows  from  a  contradiction;  call these logics explosive. Paraconsistent logics are logics that are not explosive. Paraconsistent logics have a long and fruitful history, and no doubt a long and fruitful future. To give some sense of the situation, I spend Section 1 exploring exactly what it takes for a logic to be paraconsistent. It will emerge that there is considerable open texture to the idea. In Section 2, I give some examples of techniques for developing paraconsistent logics. In Section 3, I discuss what seem to me to be some promising applications of certain paraconsistent logics. In fact, however, I don’t think there’s all that much to the concept ‘paraconsistent’ itself; the collection of paraconsistent logics is far too heterogenous to be very productively dealt with under a single label. Perhaps that will emerge as we go."))
          , C (Chapter
              "Experimental philosophical logic"
              Solo
              "A Companion to Experimental Philosophy"
              [ "Justin Sytsma"
              , "Wesley Buckwalter"
              ]
              "Wiley"
              "./papers/xpl.pdf"
              (CPD 2016 523 534 "http://www.wiley.com/WileyCDA/WileyTitle/productCd-1118661699.html")
              "ripley:xpl"
              (Just "This paper explores the intersection  of  experimental philosophy and philosophical logic. I sketch some ways in which experimental results, and empirical results more broadly, can inform and have informed debates within philosophical logic."))
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
              (CPD 2015 409 430 "http://www.springer.com/us/book/9789401796729")
              "cervr:vtpc"
              (Just "We say that a sentence A is a permissive consequence of a set X of premises whenever, if all the premises in X hold up to some standard, then A holds to some weaker standard. In this paper, we focus on a three-valued version of this notion, which we call strict-to-tolerant consequence, and discuss its fruitfulness toward a unified treatment of the paradoxes of vagueness and self-referential truth. For vagueness, st-consequence supports the principle of tolerance; for truth, it supports the requisite of transparency. Permissive consequence is non-transitive, but this feature is argued to be an essential component to the understanding of paradoxical reasoning in cases involving vagueness or self-reference."))
          , C (Chapter
              "Nonclassical theories of truth"
              (Other [ "jcBeall", "davidRipley" ])
              "the Oxford Handbook of Truth"
              [ "Michael Glanzberg" ]
              "Oxford University Press"
              "./papers/nctt.pdf"
              ForthcomingCPD
              "br:nctt"
              Nothing)
          , C (Chapter
              "Embedding denial"
              Solo
              "Foundations of Logical Consequence"
              [ "Colin Caret"
              , "Ole Hjortland"
              ]
              "Oxford University Press"
              "./papers/ed.pdf"
              (CPD 2015 289 309 "http://global.oup.com/academic/product/foundations-of-logical-consequence-9780198715696")
              "ripley:ed"
              Nothing)
          , A (Article
              "Vagueness and order effects in color categorization"
              "Journal of Logic, Language, and Information"
              (Other [ "paulEgre", "vincentDeGardelle", "davidRipley" ])
              "./papers/voe.pdf"
              (APD 2013 22 (Just 4) 391 420 "10.1007/s10849-013-9183-7")
              "egr:voe"
              (Just "This paper proposes an experimental investigation of the use of vague predicates in dynamic sorites. We present the results of two studies in which subjects had to categorize colored squares at the borderline between two color categories. Our main aim was to probe for hysteresis in the ordered transitions between the respective colors, namely for the longer persistence of the initial category. Our main finding is a reverse phenomenon of enhanced contrast (i.e. negative hysteresis), present in two different tasks, a comparative task involving two color names, and a yes/no task involving a single color name, but not found in a corresponding color matching task. We propose an optimality-theoretic explanation of this effect in terms of the strict-tolerant framework of Cobreros et al.'s \"Tolerant, classical, strict\", in which borderline cases are characterized in a dual manner in terms of overlap between tolerant extensions, and underlap between strict extensions."))
          , A (Article
              "Reaching transparent truth"
              "Mind"
              CERvR
              "http://mind.oxfordjournals.org/cgi/reprint/fzt110?ijkey=xGKyG7colWYSJzh&keytype=ref"
              (APD 2013 122 (Just 488) 841 866 "10.1093/mind/fzt110")
              "cervr:rtt"
              (Just "This paper presents and defends a way to add a transparent truth predicate to classical logic, such that T(A) and A are everywhere intersubstitutable, where all T-biconditionals hold, and where truth can be made compositional. A key feature of our framework, called STTT (for Strict-Tolerant Transparent Truth), is that it supports a non-transitive relation of consequence. At the same time, it can be seen that the only failures of transitivity STTT allows for arise in paradoxical cases."))
          , A (Article
              "Identity, Leibniz's law, and nontransitive reasoning"
              "Metaphysica"
              CERvR
              "./papers/ill.pdf"
              (APD 2013 14 (Just 2) 253 264 "10.1007/s12133-013-0125-2")
              "cervr:illntr"
              (Just "Arguments based on Leibniz's Law seem to show that there is no room for either indefinite or contingent identity. The arguments seem to prove too much, but their conclusion is hard to resist if we want to keep Leibniz's Law. We present a novel approach to this issue, based on an appropriate modification of the notion of logical consequence."))
          , A (Article
              "Revising up"
              "Philosophers' Imprint"
              Solo
              "http://quod.lib.umich.edu/cgi/p/pod/dod-idx/revising-up-strengthening-classical-logic-in-the-face.pdf?c=phimp;idno=3521354.0013.005"
              (APD 2013 13 (Just 5) 1 13 "")
              "ripley:ru"
              (Just "This paper provides a defense of the full strength of classical logic, in  a  certain  form,  against  those  who  would  appeal  to  semantic  paradox or vagueness in an argument for a weaker logic. I will not argue that these paradoxes are based on mistaken principles; the approach I  recommend  will  extend  a  familiar  formulation  of  classical  logic  by including a fully transparent truth predicate and fully tolerant vague predicates. It has been claimed that these principles are not compatible with classical logic; I will argue, by both drawing on previous work and presenting new work in the same vein, that this is not so. We can combine  classical  logic  with  these  intuitive  principles,  so  long  as  we allow the result to be nontransitive. In the end, I hope the paper will help us to handle familiar paradoxes within classical logic; along the way, I hope to shed some light on what classical logic might be for."))
          , A (Article
              "Paradoxes and failures of cut"
              "Australasian Journal of Philosophy"
              Solo
              "./papers/pafc.pdf"
              (APD 2013 91 (Just 1) 139 164 "")
              "ripley:pafc"
              (Just "This paper presents and motivates a new philosophical and logical approach to truth and semantic paradox. It begins from an inferentialist, and particularly bilateralist, theory of meaning—one which takes meaning to be constituted by assertibility and deniability conditions—and shows how the usual multiple-conclusion sequent calculus for classical logic can be given an inferentialist motivation, leaving classical model theory as of only derivative importance. The paper then uses this theory of meaning to present and motivate a logical system--—ST--—that conservatively extends classical logic with a fully transparent truth predicate. This system is shown to allow for classical reasoning over the full (truth-involving) vocabulary, but to be non-transitive. Some special cases where transitivity does hold are outlined. ST is also shown to give rise to a familiar sort of model for non-classical logics: Kripke fixed points on the Strong Kleene valuation scheme. Finally, to give a theory of paradoxical sentences, a distinction is drawn between two varieties of assertion and two varieties of denial. On one variety, paradoxical sentences cannot be either asserted or denied; on the other, they must be both asserted and denied. The target theory is compared favourably to more familiar related systems, and some objections are considered and responded to."))
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
              (CPD 2013 329 348 mempty)
              "ripley:ss"
              Nothing)
          , A (Article
              "Explaining the abstract/concrete paradoxes in moral psychology"
              "Review of Philosophy and Psychology"
              (Other [ "ericMandelbaum", "davidRipley" ])
              "./papers/nbar.pdf"
              (APD 2012 3 (Just 3) 351 368 "")
              "mr:nbar"
              (Just "For some reason, participants hold agents more responsible for their actions when a situation is described concretely than when the situation is described abstractly. We present examples of this phenomenon, and survey some attempts to explain it. We divide these attempts into two classes: affective theories and cognitive theories. After criticizing both types of theories we advance our novel hypothesis: that people believe that whenever a norm is violated, someone is responsible for it. This belief, along with the familiar workings of cognitive dissonance theory, is enough to not only explain all of the abstract/concrete paradoxes, but also explains seemingly unrelated effects, like the anthropomorphization of malfunctioning inanimate objects."))
          , A (Article
              "Structures and circumstances"
              "Synthese"
              Solo
              "./papers/sc.pdf"
              (APD 2012 189 (Just 1) 97 118 "")
              "ripley:sc"
              (Just "This paper discusses two distinct strategies that have been adopted to provide fine-grained propositions; that is, propositions individuated more finely than sets of possible worlds. One strategy takes propositions to have internal structure, while the other looks beyond possible worlds, and takes propositions to be sets of circumstances, where possible worlds do not exhaust the circumstances. The usual arguments for these positions turn on fineness-of-grain issues: just how finely should propositions be individuated? Here, I compare the two strategies with an eye to the fineness-of-grain question, arguing that when a wide enough range of data is considered, we can see that a circumstance-based approach, properly spelled out, outperforms a structure-based approach in answering the question. (Part of this argument involves spelling out what I take to be a reasonable circumstance-based approach.) An argument to the contrary, due to Soames, is also considered."))
          , A (Article
              "Tolerance and mixed consequence in the s'valuationist setting"
              "Studia Logica"
              CERvR
              "./papers/tmcsv.pdf"
              (APD 2012 100 (Just 4) 855 877 "")
              "cervr:tmcss"
              (Just "In a previous paper (‘Tolerant, Classical, Strict’), we investigated a semantic framework to deal with the idea that vague predicates are tolerant, namely that small changes do not affect the applicability of a vague predicate even if large changes do.  Our approach there rests on two main ideas.  First, given a classical extension of a predicate, we can define a strict and a tolerant extension depending on an indifference relation associated to that predicate. Second, we can use these notions of satisfaction to define mixed consequence relations that capture non-transitive tolerant reasoning. Although we gave some empirical motivation for the use of strict and tolerant extensions, making use of them commits us to the view that classical tautologies or contradictions are not automatically valid or unsatisfiable, respectively. Some philosophers might take this commitment as a negative outcome of our previous proposal.  We think, however, that the general ideas underlying our previous approach to vagueness can be implemented in a variety of ways.  This paper explores the possibility of defining mixed notions of consequence in the more classical super/sub-valuationist setting and examines to what extent any of these notions captures non-transitive tolerant reasoning."))
          , A (Article
              "Conservatively extending classical logic with transparent truth"
              "Review of Symbolic Logic"
              Solo
              "./papers/cecltt.pdf"
              (APD 2012 5 (Just 2) 354 378 "")
              "ripley:cecltt"
              (Just "This paper shows how to conservatively extend a classical logic with a transparent truth predicate, in the face of the paradoxes that arise as a consequence. All classical inferences are preserved, and indeed extended to the full (truth-involving) vocabulary. However, not all classical metainferences are preserved; in particular, the resulting logical system is nontransitive. Some limits on this nontransitivity are adumbrated, and two proof systems are presented and shown to be sound and complete. (One proof system features admissible Cut, but the other does not.)"))
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
              (APD 2012 41 (Just 3) 595 612 "")
              "ternary"
              (Just "One of the most dominant approaches to semantics for relevant (and many paraconsistent) logics is the Routley–Meyer semantics involving a ternary relation on points. To some (many?), this ternary relation has seemed like a technical trick devoid of an intuitively appealing philosophical story that connects it up with conditionality in general. In this paper, we respond to this worry by providing three different philosophical accounts of the ternary relation that correspond to three conceptions of conditionality. We close by briefly discussing a general conception of conditionality that may unify the three given conceptions."))
          , A (Article
              "Tolerant, classical, strict"
              "Journal of Philosophical Logic"
              CERvR
              "http://link.springer.com/content/pdf/10.1007%2Fs10992-010-9165-z.pdf"
              (APD 2012 41 (Just 2) 347 385 "")
              "cervr:tcs"
              (Just "In this paper we investigate a semantics for first-order logic originally proposed by R. van Rooij to account for the idea that vague predicates are tolerant, that is, for the principle that if x is P, then y should be P whenever y is  similar  enough  to x.  The  semantics,  which  makes  use  of  indifference relations to model similarity, rests on the interaction of three notions of truth: the classical notion,  and  two  dual  notions  simultaneously  defined  in  terms of it, which we call tolerant truth and strict truth. We characterize the space of consequence relations definable in terms of those and discuss the kind of solution  this  gives  to  the  sorites  paradox.  We  discuss  some  applications  of the  framework  to  the  pragmatics  and  psycholinguistics  of  vague  predicates, in particular regarding judgments about borderline cases."))
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
              (CPD 2011 41 58 "")
              "ripley:ii"
              Nothing)
          , A (Article
              "Negation, denial, and rejection"
              "Philosophy Compass"
              Solo
              "./papers/ndr.pdf"
              (APD 2011 6 (Just 9) 622 629 "")
              "ripley:ndr"
              (Just "At least since Frege and Geach, there has been some consensus about the relation between negation, the speech act of denial, and the attitude of rejection: a denial, the consensus has  had  it,  is  the  assertion  of  a  negation,  and  a  rejection  is  a  belief  in  a  negation.  Recently, though, there have been notable deviations from this orthodox view. Rejectivists have maintained that negation is to be explained in terms of denial or rejection, rather than vice versa. Some other theorists have maintained that negation is a separate phenomenon from denial, and that neither is to be explained in terms of the other. In this paper, I present and consider these heterodox theories of the relation between negation, denial, and rejection."))
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
              (CPD 2011 169 188 "")
              "ripley:catb"
              Nothing)
          , A (Article
              "Responsibility and the brain sciences"
              "Ethical Theory and Moral Practice"
              (Other [ "felipeDeBrigard", "ericMandelbaum", "davidRipley" ])
              "./papers/rbs.pdf"
              (APD 2009 12 (Just 5) 511 524 "")
              "dbmr:rbs"
              (Just "Some theorists think that the more we get to know about the neural underpinnings of our behaviors, the less likely we will be to hold people responsible for their actions. This intuition has driven some to suspect that as neuroscience gains insight into the neurological causes of our actions, people will cease to view others as morally responsible for their actions, thus creating a troubling quandary for our legal system. This paper provides empirical evidence against such intuitions. Particularly, our studies of folk intuitions suggest that (1) when the causes of an action are described in neurological terms, they are not found to be any more exculpatory than when described in psychological terms, and (2) agents are not held fully responsible even for actions that are fully neurologically caused."))
          , A (Article
              "Analetheism and dialetheism"
              "Analysis"
              (Other [ "jcBeall", "davidRipley" ])
              "./papers/ad.pdf"
              (APD 2004 64 (Just 1) 30 35 "")
              "br:ad"
              Nothing)
          ]
-}