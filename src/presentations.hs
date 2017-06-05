{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Presentations (Presentation(..), extrasMarks, presentations) where

import Data.Text (Text)
import Data.Monoid (mempty, (<>))
import Lucid

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Yaml.Aeson as Y
import qualified Data.ByteString as BS

import WebsiteTools (AuthorCat(..), listItems, lk, pileUp)
import Links

data Link = Link Text (Html ()) 

data PresExtraType = Slides | Handout | Video | OtherExtra deriving (Eq)


type PELinkURL = Text
type PELinkWord = Text
data PresExtras = PE PresExtraType PELinkURL PELinkWord

data Presentation = P { presTitle :: Text
                      , presAuthors :: AuthorCat
                      , presLocations :: [ Text ]
                      , presExtras :: [ PresExtras ]
                      }

deriveJSON defaultOptions ''PresExtraType
deriveJSON defaultOptions ''PresExtras
deriveJSON defaultOptions ''Presentation

extraMark :: PresExtraType -> Html ()
extraMark pe = span_ [class_ ("fa fa-fw " <> pec)] ""
  where pec = case pe of
                   Slides     -> "fa-desktop"
                   Handout    -> "fa-paper-plane" --file-text-o, paper-plane
                   Video      -> "fa-video-camera" --video-camera, film
                   OtherExtra -> "fa-chain"

extraRow :: PresExtras -> Html ()
extraRow (PE ty ur tx) =
    (tr_ $ do
       (td_ [class_ "contact-icon"]  (a_ [href_ ur, target_ "_blank"] (extraMark ty)))
       (td_ (a_ [href_ ur, target_ "_blank"] $ toHtml tx)))

extrasMarks :: Presentation -> Html ()
extrasMarks p
  | null pes = mempty
  | otherwise = div_ [class_ "col-md-2 extra-marks"]
                    (table_ $ (pileUp $ map extraRow pes))
  where pes = presExtras p

presFile :: FilePath
presFile = "./src/presentations.yaml"

presentations :: IO (Maybe [Presentation])
presentations = do
  pData <- BS.readFile presFile
  return (Y.decode pData)

{-
presentations :: [Presentation]
presentations =
  [ P "Conflation: logic and applications"
      Solo
      [ "Monash University, June 2017"
      , "University of Melbourne, May 2017"
      , "University of Sydney, May 2017"
      ]
      [ PE Slides "./docs/cla-slides.pdf" "Slides" ]
  , P "Uncut"
      Solo
      [ "Logics of Consequence, Concordia, March 2017"
      , "University of Auckland, March 2017"
      , "University of Navarra, May 2017" ]
      [ PE Slides "./docs/uncut-slides.pdf" "Slides" ]
  , P "There is such a thing as a substructural approach to paradox"
      Solo
      [ "Non-classical solutions to the paradoxes, MCMP, February 2017" ]
      []
  , P "On the supposed unity of soritical and semantic paradox"
      Solo
      [ "Diaphora workshop, MCMP, December 2016" ]
      [ PE Slides "./docs/unity-slides-mcmp.pdf" "Slides" ]
  , P "Paradoxes and the structure of reasoning"
      Solo
      [ "Alice Ambrose Lazerowitz/Thomas Tymoczko Logic Lecture, Smith College, December 2016" ]
      [ PE Slides "./docs/psr-slides-smith.pdf" "Slides" ]
  , P "Naive validity"
      Solo
      [ "SADAF, November 2016" ]
      [ PE Slides "./docs/nv-slides.pdf" "Slides" ]
  , P "Methods of dogwhistling"
      Solo
      [ "AAP 2016, Monash University, July 2016" ]
      [ PE Slides "./docs/dw-slides-aap.pdf" "Slides" ]
  , P "Towards a naive type theory"
      Solo
      [ "History and Philosophy of Logic Session, ASL North American meeting 2016, University of Connecticut, May 2016"
      , "Australasian Association for Logic 2016 meeting, July 2016"
      ]
      [ PE Slides "./docs/tntt-slides-aal.pdf" "Slides" ]
  , P "Vagueness is a kind of conflation"
      Solo
      [ "Society for Exact Philosophy, University of Miami, May 2016" ]
      []
  , P "Conditionals with impossible antecedents"
      Solo
      [ "Experimental work in formal semantics, Pacific APA, April 2016" ]
      [ PE Handout "./docs/cia-handout-apa.pdf" "Handout" ]
  , P "Classical recapture via conflation"
      Solo
      [ "Logic and Metaphysics Workshop, CUNY Graduate Center, February 2016" ]
      []
  , P "Dialetheism is an empirical hypothesis"
      Solo
      [ "20th Amsterdam Colloquium, Amsterdam, December 2015" ]
      [ PE Slides "./docs/deh-slides-ac.pdf" "Slides" ]
  , P "\'Consequentialism\'?"
      Solo
      [ "Inferentialism Workshop, Arche, University of St. Andrews, November 2015" ]
      [ PE Slides "./docs/cons-slides-arche.pdf" "Slides" ]
  , P "Axiomatisation without cut"
      Solo
      [ "SADAF, Buenos Aires, August 2015" ]
      []
  , P "Commitment consequence"
      Solo
      [ "SADAF, Buenos Aires, August 2015" ]
      []
  , P "Vagueness, tolerance, and substructural logic"
      Solo
      [ "AAP 2015, Macquarie University, July 2015" ]
      []
  , P "Uniqueness without reflexivity or transitivity"
      Solo
      [ "Non-classical Abstract Logics, Unilog 5, June 2015"
      , "Melbourne Logic Group, University of Melbourne, October 2011"
      ]
      []      
  , P "'Transitivity'"
      Solo
      [ "AAL 2015, University of Sydney, July 2015"
      , "GroLog, University of Groningen, June 2015"
      ]
      [ PE Slides "./docs/transitivity-slides-aal.pdf" "Slides"
      , PE Handout "./docs/transitivity-handout-gro.pdf" "Handout"
      ]
  , P "What is the self?"
      (Other [ "killripLemistery" ])
      [ "Tinnie Talks, Arena, Melbourne, May 2015"
      ]
      []      
  , P "Meaning, bounds, social kinds"
      Solo
      [ "ANU Philsoc Seminar, May 2015"
      ]
      [ PE Slides "./docs/mbsk-slides-anu.pdf" "Slides" ]
  , P "From conversation to inference, via commitment"
      Solo
      [ "Charles Sturt University, May 2015" ]
      [ PE Slides "./docs/cic-slides-csu.pdf" "Slides" ]
  , P "Commitment and implicit assertion"
      Solo
      [ "De La Salle University, April 2015"
      , "University of Melbourne, April 2015"
      , "Melbourne Logic Group, University of Melbourne, August 2014"
      ]
      [ PE Slides "./docs/cia-slides-dlsu.pdf" "Slides" ]
  , P "Setting the bounds"
      (Solo)
      [ "University of Waikato, March 2015"
      , "Victoria University of Wellington, March 2015"
      , "University of Auckland, March 2015"
      ]
      [ PE Slides "./docs/stb-slides-wellington.pdf" "Slides" ]      
  , P "Contraction and closure"
      Solo
      [ "Melbourne Logic Group, University of Melbourne, April 2015"
      , "Pukeko Logic Group, March 2015"
      ]
      []
  , P "Tolerance and degrees of truth"
      CERvR
      [ "Vagueness via nonclassical logics, University of Sydney, December 2014"
      , "Trivalent Logics and their Applications, ESSLLI 2012, August 2012"
      ]
      []
  , P "What do the liar and sorites have in common?"
      Solo
      [ "AAP 2014, ANU, July 2014" ]
      []
  , P "Why I am not a noncontractivist"
      Solo
      [ "SILFS Satellite Workshop, Roma Tre University, June 2014" ]
      [ PE Slides "./docs/whynot-slides-silfs.pdf" "Slides" ]
  , P "Conflation and contradiction"
      Solo
      [ "Paraconsistent Reasoning in Science and Mathematics, MCMP, June 2014"
      , "Symposium on the principle of non-contradiction, Pacific APA, April 2014" ]
      []      
  , P "Contractions of noncontractive consequence relations"
      (Other [ "rohanFrench", "davidRipley" ])
      [ "UConn Logic Group, UConn, January 2014"
      , "Truth and Paradox Workshop, MCMP, May 2013"
      ]
      [ PE Video "https://itunes.apple.com/itunes-u/id654728467" "Video (no. 38)"
      , PE Handout "./docs/cncr_handout.pdf" "Handout (needed with video)"
      ]
  , P "Nonclassical logics (mini-course)"
      Solo
      [ "James Madison University, May 2014" ]
      [ PE Handout "./docs/jmu-handouts.pdf" "Handouts" ]
  , P "Confusion, collapse, tolerance, borderlines"
      Solo
      [ "VII Navarra Workshop on Vagueness, Universidad de Navarra, December 2013"
      , "Vagueness Seminar, NYU, November 2013"
      ]
      []
  , P "Nonmonotonicity and partiality"
      CERvR
      [ "Substructural Approaches to Paradox, Universitat de Barcelona, November 2013" ]
      []
  , P "Avoiding ad hoc approaches to paradox"
      Solo
      [ "AAP 2013, University of Queensland, July 2013" ]
      []
  , P "63 negations"
      Solo
      [ "Presidential address, AAL, University of Melbourne, June 2013"
      ]
      [ PE Slides "./docs/63neg-slides-aal.pdf" "Slides" ]
  , P "Confusion and collapse"
      Solo
      [ "University of Sydney, May 2013"
      , "Melbourne Logic Group, University of Melbourne, March 2013"
      , "University of Auckland, March 2013"
      , "Victoria University of Wellington, March 2013"
      , "University of Otago, March 2013"
      ]
      []
  , P "Multigrain entailment"
      Solo
      [ "AAPNZ 2012, Victoria University of Wellington, December 2012"
      , "2nd Propositions and Same-Saying Workshop, University of Sydney, July 2010"
      ]
      []
  , P "Bilateralism, coherence, warrant"
      Solo
      [ "University of Melbourne, October 2012"
      , "Charles Sturt University, September 2012"
      ]
      []
  , P "Some substructural arithmetic"
      (Other [ "oleHjortland", "davidRipley" ])
      [ "Melbourne Logic Group, University of Melbourne, September 2012" ]
      []
  , P "Comparing substructural theories of truth"
      Solo
      [ "Melbourne Logic Group, University of Melbourne, September 2012" ]
      []
  , P "Vagueness and hysteresis"
      (Other [ "paulEgre", "vincentDeGardelle", "davidRipley" ])
      [ "Logic and Cognition, ESSLLI 2012, August 2012" ]
      []
  , P "Anything goes"
      Solo
      [ "AAP 2012, University of Wollongong, July 2012"
      , "Paradox and Logical Revision, MCMP, July 2012"
      ]
      [ PE Video "https://itunes.apple.com/us/itunes-u/mcmp-mathematical-philosophy/id439913748" "Video (no. 59)" ]
  , P "Vagueness as confusion"
      Solo
      [ "Universiteit Gent, July 2012"
      , "Pamplona Workshop for Vagueness and Similarity, Universidad de Navarra, May 2012"
      ]
      []
  , P "Naive set theory and nontransitive logic"
      Solo
      [ "Melbourne Logic Group, University of Melbourne, March 2012" ]
      []
  , P "Vagueness, tolerance, contradiction"
      Solo
      [ "Northern Illinois University, February 2012"
      , "Yale University, January 2012"
      , "University of Connecticut, January 2012"
      , "University of Otago, October 2011"
      ]
      []
  , P "Coordination and propositions"
      Solo
      [ "Australian Metaphysics Conference, ANU Kioloa, November 2011" ]
      []
  , P "Bilateralism and paradox"
      Solo
      [ "Massey University, August 2011"
      , "AAP 2011, University of Otago, July 2011"
      ]
      []
  , P "Tonk, tolerance, and nontransitivity"
      Solo
      [ "MCMP, August 2011"
      ]
      [ PE Video "https://itunes.apple.com/itunes-u/id439913748" "Video (no. 211)" ]
  , P "Nonclassical theories of vagueness (mini-course)"
      CERvR
      [ "ESSLLI 2011, August 2011" ]
      []
  , P "Tolerant truth and permissive consequence"
      CERvR
      [ "Truth at Work, IHPST, June 2011" ]
      []
  , P "Sorites and the liar"
      CERvR
      [ "7th Barcelona Workshop on Issues in the Theory of Reference, Universitat de Barcelona, June 2011" ]
      []
  , P "Harmony without cut"
      Solo
      [ "Melbourne Logic Group, University of Melbourne, May 2011" ]
      []
  , P "Revising up"
      Solo
      [ "Monash University, May 2011"
      , "ANU, May 2011"
      , "Arche, University of St. Andrews, April 2011"
      ]
      []
  , P "Tolerant and strict truth"
      CERvR
      [ "Truth be Told, ILLC, March 2011"
      , "Institut Jean-Nicod, March 2011"
      ]
      []
  , P "Circumstantialism and identity"
      Solo
      [ "AAPNZ 2010, University of Waikato, December 2010"
      , "Kioloa Metaphysics Retreat, ANU Kioloa, November 2010"
      ]
      []
  , P "Conservatively extending classical logic with transparent truth"
      Solo
      [ "Melbourne Logic Group, University of Melbourne, October 2010" ]
      []
  , P "Strict and tolerant"
      Solo
      [ "North Island Logic Group, Victoria University of Wellington, September 2010" ]
      []
  , P "Inconstancy and inconsistency"
      Solo
      [ "University of Melbourne, August 2010" ]
      []
  , P "Embedding denial"
      Solo
      [ "PALMYR 9: Logic and the Use of Language, ILLC, June 2010"
      , "Propositional Content and Proposition-Related Acts, ENS, March 2010"
      , "Logic of Denial, Arche, University of St. Andrews, 2009"
      ]
      []
  , P "Explaining the abstract/concrete paradoxes in moral psychology"
      (Other [ "ericMandelbaum", "davidRipley" ])
      [ "Institut Jean-Nicod, June 2010"
      , "Arche/CSMN Graduate Conference, University of St. Andrews, 2009"
      ]
      []
  , P "Arbitrariness, vagueness, and the liar"
      Solo
      [ "AAL, July 2010"
      , "Arche, University of St. Andrews, May 2010"
      ]
      []
  , P "Tolerant, classical, strict"
      CERvR
      [ "AAP 2010, University of New South Wales, July 2010"
      , "NIP, University of Aberdeen, May 2010"
      , "Vagueness and Similarity, Insitut Jean-Nicod, May 2010"
      ]
      []
  , P "Against structured propositions"
      Solo
      [ "Institut Jean-Nicod, April 2010"
      , "Semantics and Philosophy in Europe 3, ENS/IHPST, May 2010"
      , "AAP 2009, University of Melbourne, 2009"
      ]
      []
  , P "Semantic possibility"
      Solo
      [ "Logos, Universitat de Barcelona, February 2010"
      , "1st Propositions and Same-Saying Workshop, Macquarie University, January 2010"
      ]
      []
  , P "Contradictions at the borders"
      Solo
      [ "2nd Paris-Barcelona Picasso Workshop, ENS, 2009"
      , "Amsterdam Graduate Philosophy Conference, Universiteit van Amsterdam, 2009"
      , "Melbourne Logic Group, University of Melbourne, 2009"
      , "Philosophy and Psychology of Vagueness, Institut Jean-Nicod, 2008"
      ]
      []
  , P "Sorting out the sorites"
      Solo
      [ "Carnegie Mellon University, 2008"
      , "University of Queensland, 2008"
      , "4th World Congress of Paraconsistency, University of Melbourne, 2008"
      ]
      []
  , P "Weak negations and neighborhood semantics"
      Solo
      [ "Logica 2010, June 2010"
      , "Melbourne Logic Group, University of Melbourne, 2008" ]
      []
  , P "Responsibility and the brain sciences"
      (Other [ "felipeDeBrigard", "ericMandelbaum", "davidRipley" ])
      [ "Ethical Theory and Moral Practice, Vrije Universiteit Amsterdam, 2008"
      , "Mind, Brain, and Experience, University of Colorado, Denver, 2008"
      ]
      []      
  ]

-}