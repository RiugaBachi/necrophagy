{-# LANGUAGE 
  NoMonoLocalBinds, RebindableSyntax, DataKinds,
  OverloadedStrings, TypeOperators, TypeApplications,
  PartialTypeSignatures, NoStarIsType
#-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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
  Bar $ O @(Q 8 > Q 8)
      # R @2 @(9^6 - PM (9-6-9-6-9-6))
  Bar $ O @(Q 8 > Q 8)
      # R @4 @(9\\\PM (7-7)///11\\\PM (7-7)///12\\\PM 7)
  Bar $ O @(Q 8 > Q 8)
      # R @2 @(9^6 - PM (9-6-9-6-9-6))
  Bar $ O @(Q 8 > T 8 > T 8)
      # R @4 @(9\\\PM (7-7)///12\\10^14^10^14^10//12)
  Bar $ O @(Q 8 > Q 8)
      # R @4 @(11^8 \\\ PM (9-6-9-6-9-6))
  Bar $ O @(Q 8 > Q 8)
      # R @4 @(9\\\PM (7-7)///11\\\PM (7-7)///12\\\PM 7)
  Sig @(3/4)
  Bar $ O @(Q 8 > 8 > 8)
      # R @2 @(9^6-7///12\\\7-Sl 9)
  Sig @(4/4)
  Bar $ O @(Q 8 > Q 8)
      # R @3 @(10\\\PM (8-8)///10\\\PM (8-8-8-8))
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
  Sig @(4/4)
  Bar $ O @(4 > 2 > T 8)
      # R @6 @(Vb (12-H)-17-17-17)
  Sig @(3/4)
  Bar $ O @(4 > T 8 > 4)
      # R @5 @(17-H-H-H-Gh (Bd (BenRelC (1/2)) 17))
  Bar $ O @(T 8 > T 8 > T 8)
      # R @5 @(18//16-19-22-19-16\\18\\16//18)
  Bar $ O @(T 8 > T 8 > 4)
      # R @6 @(16-19-22-19-16\\18\\Vb 16)
  Sig @(4/4)
  Bar $ O @(T 8 > T 8 > T 8 > T 8)
      # R @6 @(17-16-13-16-13\\17//13\\17-15-17-15-13)
  Bar $ O @(T 8 > T 8 > T 8 > 4)
      # R @5 @(15-13\\16//13\\16-14-13-14-16-Vb 13)
