{-# LANGUAGE 
  NoMonoLocalBinds, RebindableSyntax, DataKinds,
  OverloadedStrings, TypeOperators, TypeApplications,
  PartialTypeSignatures, NoStarIsType
#-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module SecondStone where

import Necrophagy

secondStone :: Tablature
secondStone = Tablature meta tracks
  where
    meta = TabMeta
      { tabAuthor = "Riuga"
      , tabArtist = "Epica"
      , tabName   = "The Second Stone"
      , tabDifficulty = "Intermediate"
      , tabComments   = mempty
      }

    tracks = TrackList @Flexible
      [ Track
          { trackName    = "Rhythm Guitar 1"
          , trackProgram = DistortionGuitar `Tuned` EStandard @6
          , trackBody    = rhythmGuitar1
          }
      ]

rhythmGuitar1 :: _
rhythmGuitar1 = do
  Tempo @177
  Sig @(4/4)
  -- ...
  Bar $ O @(Q 8 > Q 8)
      # R @2 @(7-Sl H \\ PM ((0*2)-(0*2)-M-(0*2)-M-(0*2)))
  -- (Chorus)
  Bar $ O @(1)
      # R @1 @(0*2)
  Bar $ O @(2 > 4 > 4)
      # L @("As" > "time," > "goes")
      # R @2 @((5*7*7)-(4*7*6)\\(5*7*7))
  Bar $ O @(1)
      # L @("by")
      # R @1 @(0*2*2)
  Bar $ O @(2 > P 4 > 8)
      # L @("I" > "hide" > "the")
      # R @1 @((3*5*5)//(2*4*4)\\(0*2))
  Bar $ O @(1)
      # L @("truth.")
      # R @1 @(0*2*2)
  Bar $ O @(2 > 4 > 4)
      # L @("Cannot stand" > "my-self" > "in this")
      # R @2 @((5*7*7)-(4*7*6)\\(5*7*7))
  Bar $ O @(1)
      # R @1 @(0*2*2)
  Bar $ O @(2 > 4 > 4)
      # L @("bro-" > "-ken" > "shell")
      # R @1 @((3*5*5)//(2*4*4)-(2*4*4))
    
