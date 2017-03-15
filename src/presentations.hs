{-# LANGUAGE OverloadedStrings #-}

module Presentations (Presentation(..), extrasMarks, presentations) where

import Data.Text (Text)
import Data.Monoid (mempty, (<>))
import Lucid

import WebsiteTools (AuthorCat(..), listItems, lk, pileUp)
import Links

data Link = Link Text (Html ()) 

data PresExtraType = Slides | Handout | Video | OtherExtra deriving (Eq)

data PresExtras = PE PresExtraType Link 

data Presentation = P { presTitle :: Text
                      , presAuthors :: AuthorCat
                      , presLocations :: [ Html () ]
                      , presExtras :: [ PresExtras ]
                      } 

extraMark :: PresExtraType -> Html ()
extraMark pe = span_ [class_ ("fa fa-fw " <> pec)] ""
  where pec = case pe of
                   Slides     -> "fa-desktop"
                   Handout    -> "fa-paper-plane" --file-text-o, paper-plane
                   Video      -> "fa-video-camera" --video-camera, film
                   OtherExtra -> "fa-chain"

extraRow :: PresExtras -> Html ()
extraRow (PE ty (Link ur tx)) =
    (tr_ $ do
       (td_ [class_ "contact-icon"]  (a_ [href_ ur, target_ "_blank"] (extraMark ty)))
       (td_ (a_ [href_ ur, target_ "_blank"] tx)))

extrasMarks :: Presentation -> Html ()
extrasMarks p
  | null pes = mempty
  | otherwise = div_ [class_ "col-md-2 extra-marks"]
                    (table_ $ (pileUp $ map extraRow pes))
  where pes = presExtras p

presentations :: [Presentation]
presentations =
  [ P "Uncut"
      Solo
      [ "Logics of Consequence, Concordia, March 2017"
      , "University of Auckland, March 2017"
      , "University of Sydney, May 2017"
      , "University of Navarra, May 2017" ]
      [ PE Slides (Link "./docs/uncut-slides.pdf" "Slides") ]
  , P "There is such a thing as a substructural approach to paradox"
      Solo
      [ "Non-classical solutions to the paradoxes, MCMP, February 2017" ]
      []
  , P "On the supposed unity of soritical and semantic paradox"
      Solo
      [ "Diaphora workshop, MCMP, December 2016" ]
      [ PE Slides (Link "./docs/unity-slides-mcmp.pdf" "Slides") ]
  , P "Paradoxes and the structure of reasoning"
      Solo
      [ "Alice Ambrose Lazerowitz/Thomas Tymoczko Logic Lecture, Smith College, December 2016" ]
      [ PE Slides (Link "./docs/psr-slides-smith.pdf" "Slides") ]
  , P "Naive validity"
      Solo
      [ "SADAF, November 2016" ]
      [ PE Slides (Link "./docs/nv-slides.pdf" "Slides") ]
  , P "Methods of dogwhistling"
      Solo
      [ aapLinkFull <> " 2016, Monash University, July 2016" ]
      [ PE Slides (Link "./docs/dw-slides-aap.pdf" "Slides") ]
  , P "Towards a naive type theory"
      Solo
      [ "History and Philosophy of Logic Session, ASL North American meeting 2016, University of Connecticut, May 2016"
      , "Australasian Association for Logic 2016 meeting, July 2016"
      ]
      [ PE Slides (Link "./docs/tntt-slides-aal.pdf" "Slides") ]
  , P "Vagueness is a kind of conflation"
      Solo
      [ "Society for Exact Philosophy, University of Miami, May 2016" ]
      []
  , P "Conditionals with impossible antecedents"
      Solo
      [ "Experimental work in formal semantics, Pacific APA, April 2016" ]
      [ PE Handout (Link "./docs/cia-handout-apa.pdf" "Handout") ]
  , P "Classical recapture via conflation"
      Solo
      [ "Logic and Metaphysics Workshop, CUNY Graduate Center, February 2016" ]
      []
  , P "Dialetheism is an empirical hypothesis"
      Solo
      [ (lk "http://www.illc.uva.nl/AC/AC2015/" "20th Amsterdam Colloquium") <> ", Amsterdam, December 2015" ]
      [ PE Slides (Link "./docs/deh-slides-ac.pdf" "Slides") ]
  , P "\'Consequentialism\'?"
      Solo
      [ "Inferentialism Workshop, Arche, University of St. Andrews, November 2015" ]
      [ PE Slides (Link "./docs/cons-slides-arche.pdf" "Slides") ]
  , P "Axiomatisation without cut"
      Solo
      [ (lk "http://http://www.sadaf.org.ar" "SADAF") <> ", Buenos Aires, August 2015" ]
      []
  , P "Commitment consequence"
      Solo
      [ (lk "http://http://www.sadaf.org.ar" "SADAF") <> ", Buenos Aires, August 2015" ]
      []
  , P "Vagueness, tolerance, and substructural logic"
      Solo
      [ aapLinkFull <> " 2015, Macquarie University, July 2015" ]
      []
  , P "Uniqueness without reflexivity or transitivity"
      Solo
      [ (lk "http://www.uni-log.org/wk5-NCAL.html" "Non-classical Abstract Logics")
        <> ", " <>
        (lk "http://www.uni-log.org/start5.html" "Unilog 5")
        <> ", June 2015"
      , "Melbourne Logic Group, University of Melbourne, October 2011"
      ]
      []      
  , P "'Transitivity'"
      Solo
      [ aalLinkFull <> " 2015, University of Sydney, July 2015"
      , "GroLog, University of Groningen, June 2015"
      ]
      [ PE Slides (Link "./docs/transitivity-slides-aal.pdf" "Slides")
      , PE Handout (Link "./docs/transitivity-handout-gro.pdf" "Handout")
      ]
  , P "What is the self?"
      (Other [ "killripLemistery" ])
      [ (lk "http://arena.org.au/tag/tinnie-talks" "Tinnie Talks")
        <> ", " <>
        (lk "http://arena.org.au" "Arena")
        <> ", Melbourne, May 2015"
      ]
      []      
  , P "Meaning, bounds, social kinds"
      Solo
      [ (lk "http://philrsss.anu.edu.au/regular-seminars/philsoc-seminars"
            "ANU Philsoc Seminar") <> ", May 2015"
      ]
      [ PE Slides (Link "./docs/mbsk-slides-anu.pdf" "Slides") ]
  , P "From conversation to inference, via commitment"
      Solo
      [ "Charles Sturt University, May 2015" ]
      [ PE Slides (Link "./docs/cic-slides-csu.pdf" "Slides") ]
  , P "Commitment and implicit assertion"
      Solo
      [ "De La Salle University, April 2015"
      , "University of Melbourne, April 2015"
      , "Melbourne Logic Group, University of Melbourne, August 2014"
      ]
      [ PE Slides (Link "./docs/cia-slides-dlsu.pdf" "Slides") ]
  , P "Setting the bounds"
      (Solo)
      [ "University of Waikato, March 2015"
      , "Victoria University of Wellington, March 2015"
      , "University of Auckland, March 2015"
      ]
      [ PE Slides (Link "./docs/stb-slides-wellington.pdf" "Slides") ]      
  , P "Contraction and closure"
      Solo
      [ "Melbourne Logic Group, University of Melbourne, April 2015"
      , (lk "https://sites.google.com/site/otagologic/logic-in-nz" "Pukeko Logic Group")
        <> ", March 2015"
      ]
      []
  , P "Tolerance and degrees of truth"
      CERvR
      [ (lk "http://sydney.edu.au/arts/philosophy/research/conferences.shtml" "Vagueness via nonclassical logics") <> ", University of Sydney, December 2014"
      , "Trivalent Logics and their Applications, ESSLLI 2012, August 2012"
      ]
      []
  , P "What do the liar and sorites have in common?"
      Solo
      [ aapLinkFull <> " 2014, ANU, July 2014" ]
      []
  , P "Why I am not a noncontractivist"
      Solo
      [ "SILFS Satellite Workshop, Roma Tre University, June 2014" ]
      [ PE Slides (Link "./docs/whynot-slides-silfs.pdf" "Slides") ]
  , P "Conflation and contradiction"
      Solo
      [ (lk "http://www.paraconsistency2014.philosophie.uni-muenchen.de/index.html"
            "Paraconsistent Reasoning in Science and Mathematics")
        <> ", " <> mcmpLinkFull <> ", June 2014"
      , "Symposium on the principle of non-contradiction, Pacific APA, April 2014" ]
      []      
  , P "Contractions of noncontractive consequence relations"
      (Other [ "rohanFrench", "davidRipley" ])
      [ (lk "http://logic.uconn.edu/" "UConn Logic Group") <> ", UConn, January 2014"
      , (lk "http://www.paradoxandtruth2013.philosophie.uni-muenchen.de/index.html"
            "Truth and Paradox Workshop")
        <> ", " <> mcmpLinkFull <> ", May 2013"
      ]
      [ PE Video (Link "https://itunes.apple.com/itunes-u/id654728467" "Video (no. 38)")
      , PE Handout (Link "./docs/cncr_handout.pdf" "Handout (needed with video)")
      ]
  , P "Nonclassical logics (mini-course)"
      Solo
      [ "James Madison University, May 2014" ]
      [ PE Handout (Link "./docs/jmu-handouts.pdf" "Handouts") ]
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
      [ aapLinkFull <> " 2013, University of Queensland, July 2013" ]
      []
  , P "63 negations"
      Solo
      [ "Presidential address, " <> aalLinkFull
        <> ", University of Melbourne, June 2013"
      ]
      [ PE Slides (Link "./docs/63neg-slides-aal.pdf" "Slides") ]
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
      [ aapLinkFull <> " 2012, University of Wollongong, July 2012"
      , (lk "https://sites.google.com/site/mcmpparadox2012/home" "Paradox and Logical Revision")
        <> ", " <> mcmpLinkFull <> ", July 2012"
      ]
      [ PE Video (Link "https://itunes.apple.com/us/itunes-u/mcmp-mathematical-philosophy/id439913748" "Video (no. 59)") ]
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
      , aapLinkFull <> " 2011, University of Otago, July 2011"
      ]
      []
  , P "Tonk, tolerance, and nontransitivity"
      Solo
      [ mcmpLinkFull <> ", August 2011"
      ]
      [ PE Video (Link "https://itunes.apple.com/itunes-u/id439913748" "Video (no. 211)") ]
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
      , ijnLinkFull <> ", March 2011"
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
      [ ijnLinkFull <> ", June 2010"
      , "Arche/CSMN Graduate Conference, University of St. Andrews, 2009"
      ]
      []
  , P "Arbitrariness, vagueness, and the liar"
      Solo
      [ (aalLink "Australasian Association for Logic") <> ", July 2010"
      , "Arche, University of St. Andrews, May 2010"
      ]
      []
  , P "Tolerant, classical, strict"
      CERvR
      [ (aapLink "AAP") <> " 2010, University of New South Wales, July 2010"
      , "NIP, University of Aberdeen, May 2010"
      , "Vagueness and Similarity, Insitut Jean-Nicod, May 2010"
      ]
      []
  , P "Against structured propositions"
      Solo
      [ ijnLinkFull <> ", April 2010"
      , "Semantics and Philosophy in Europe 3, ENS/IHPST, May 2010"
      , (aapLink "AAP") <> " 2009, University of Melbourne, 2009"
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
      , "Philosophy and Psychology of Vagueness, " <> ijnLinkFull <> ", 2008"
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
