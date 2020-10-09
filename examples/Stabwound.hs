{-# LANGUAGE 
  NoMonoLocalBinds, RebindableSyntax, DataKinds,
  OverloadedStrings, TypeOperators, TypeApplications,
  PartialTypeSignatures, NoStarIsType
#-}

module Stabwound where

import Necrophagy

stabwound :: Tablature
stabwound = Tablature meta tracks
  where
    meta = TabMeta 
      { tabAuthor = "Riuga"
      , tabArtist = "Necrophagist"
      , tabName   = "Stabwound"
      , tabDifficulty = "Expert"
      , tabComments   = mempty
      }

    tracks = TrackList @Flexible
      [ Track 
          { trackName    = "Rhythm Guitar - Muhammed Suicmez"
          , trackProgram = DistortionGuitar `Tuned` DStandard @6
          , trackBody    = suicmez
          }
      ]

suicmez :: _
suicmez = do
  Tempo @240
  Sig @(4/4)
  Bar $ O @(8 > 8 > 8 > 8 > 8 > 8 > 8 > 8)
      # R @2 @(9^6 - PM (9-6-9-6-9-6))
  Bar $ O @(8 > 8 > 8 > 8 > 8 > 8 > 8 > 8)
      # R @4 @(9\\\PM (7-7)///11\\\PM (7-7)///12\\\PM 7)
  -- ...
  Marker @"First sweep"
  Bar $ O @(4 > T 8 > T 8 > T 8)
      # R @2 @(M-19//17//16//17//15-19-24-19-15)
  Bar $ O @(T 8 > 2 > 4)
      # R @5 @(17\\16\\17\\ Vb (19-Sl H))
  Sig @(5/4)
  Bar $ O @(T 8 > T 8 > T 8 > T 8 > T 8)
      # R @2 @(8-11-12//9-10-12//9-10-13//10-12-13//10-12-13)
  Bar $ O @(Q 16 > Q 16 > Q 16 > Q 16 > T 8)
      # R @6 @(Rep 3 (17-13-12-10) - (20-13-12-10) - (20-16-13))
