{-# LANGUAGE 
  NoMonoLocalBinds, RebindableSyntax, DataKinds,
  OverloadedStrings, TypeOperators, TypeApplications,
  PartialTypeSignatures, NoStarIsType
#-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Indifferent where

import Necrophagy

indifferent :: Tablature
indifferent = Tablature meta tracks
  where
    meta = TabMeta
      { tabAuthor = "Riuga"
      , tabArtist = "Destiny Potato"
      , tabName   = "Indifferent"
      , tabDifficulty = "Intermediate"
      , tabComments   = mempty
      }

    tracks = TrackList @Flexible
      [ Track
          { trackName    = "Rhythm Guitar 1"
          , trackProgram = DistortionGuitar `Tuned` DropAb @6
          , trackBody    = rhythmGuitar1
          }
      ]

rhythmGuitar1 :: _
rhythmGuitar1 = do
  Tempo @140
  Sig @(4/4)
  -- Intro
  Bar $ O @(T 8 > T 8 > T' 8 > T' 16 > T' 16 > T' 8 > T 8)
      # R @1 @(2^4^7//7^9///7\\9^11^9^7//7^9^12)
  Bar $ O @(T 8 > 4 > T 8 > 4)
      # R @4 @(Rep 4 (12*11*10) - Rep 4 (7*6*5))
  Bar $ O @(T 8 > T 8 > P (T' 8) > T 16 > 4)
      # R @1 @(0-(0)-(0)///4//0//2\\4-5-4-2-0)
  Bar $ O @(T 8 > T 8 > 2)
      # R @1 @(2^4//2^4//2//2-(H*2*2))
  Bar $ O @(T 8 > T 8 > T' 8 > T' 16 > T' 16 > T' 8 > T 8)
      # R @1 @(2^4^7//7^9///7\\9^11^9^7//7^9^12)
  Bar $ O @(T 8 > 4 > T 8 > 4)
      # R @4 @(Rep 4 (12*11*10) - Rep 4 (7*6*5))
  Bar $ O @(T 8 > T 8 > P (T' 8) > T 16 > 4)
      # R @1 @(0-(0)-(0)///4//0//2\\4-5-4-2-0)
  Bar $ O @(T 8 > T 8 > 2)
      # R @1 @(2^4//2^4//2//2-(H*2*2))
  -- Chorus
  Bar $ O @(T 8 > T 8 > T 8 > T 8)
      # R @1 @(2-4-4-4-X-X-2-4-4-4-X-X)
  Bar $ O @(T 8 > T 8 > T 8 > T 8)
      # R @1 @(2-4-4-4-X-X-2-4-4-4-X-X)
  Bar $ O @(T 8 > T 8 > T 8 > T 8)
      # R @1 @(2-4-4-4-X-X-2-4-4-4-X-X)
  Bar $ O @(T 8 > T 8 > T 8 > 4)
      # R @1 @(4^7//4^7^9///7^9^12//11//10)
  Bar $ O @(2 > T 4)
      # R @1 @((0*0*0)-Arp (2 `On` 6 - 2 `On` 5 - 0 `On` 4))
  Bar $ O @(2 > T 4)
      # R @2 @((2*4*2)-Arp (4 `On` 6 - 2 `On` 5 - 2 `On` 4))
  Bar $ O @(2 > T 4)
      # R @1 @((4*4*4)-Arp (5 `On` 6 - 6 `On` 5 - 7 `On` 4))
  Bar $ O @(2 > T 4)
      # R @1 @((7*7*7)-Arp (9 `On` 6 - 9 `On` 5 - 7 `On` 4))
  Bar $ O @(2 > T 4)
      # R @1 @((0*0*0)-Arp (2 `On` 6 - 2 `On` 5 - 0 `On` 4))
  Bar $ O @(2 > T 4)
      # R @2 @((2*4*2)-Arp (4 `On` 6 - 2 `On` 5 - 2 `On` 4))
  Bar $ O @(2 > T 4)
      # R @1 @((4*4*4)-Arp (5 `On` 6 - 6 `On` 5 - 7 `On` 4))
  Bar $ O @(2 > T 4)
      # R @1 @((7*7*7)-Arp (9 `On` 6 - 9 `On` 5 - 7 `On` 4))
  -- Bridge
  Bar $ O @(T 8 > T 8 > 2)
      # R @1 @((0*0*0)-0-0-0-0-0-(0*0*0*0))
  Bar $ O @(T 8 > T 8 > 2)
      # R @2 @((2*4*2)-2-2-2-2-2-(2*4*2*4))
  Bar $ O @(1)
      # R @1 @(4*4*4*7*6*5)
  Bar $ O @(T 4 > 2)
      # R @2 @(Arp ((4 `On` 2) - (2 `On` 3) - (4 `On` 4) - (2 `On` 5)))
  Bar $ O @(T 4 > 2)
      # R @2 @(Arp ((2 `On` 2) - (0 `On` 3) - (0 `On` 4) - (0 `On` 5)))
  Bar $ O @(T 4 > 2)
      # R @2 @(Arp ((4 `On` 2) - (2 `On` 3) - (4 `On` 4) - (2 `On` 5)))
  -- Outro
  Bar $ O @(T 8 > T 8 > T' 8 > T' 16 > T' 16 > T' 8 > T 8)
      # R @1 @(2^4^7//7^9///7\\9^11^9^7//7^9^12)
  Bar $ O @(T 8 > 4 > T 8 > 4)
      # R @4 @(Rep 4 (12*11*10) - Rep 4 (7*6*5))
  Bar $ O @(T 8 > T 8 > P (T' 8) > T 16 > 4)
      # R @1 @(0-(0)-(0)///4//0//2\\4-5-4-2-0)
  Bar $ O @(T 8 > T 8 > 2)
      # R @1 @(2^4//2^4//2//2-(H*2*2))
  Bar $ O @(T 8 > T 8 > T' 8 > T' 16 > T' 16 > T' 8 > T 8)
      # R @1 @(2^4^7//7^9///7\\9^11^9^7//7^9^12)
  Bar $ O @(T 8 > 4 > T 8 > 4)
      # R @4 @(Rep 4 (12*11*10) - Rep 4 (7*6*5))
  Bar $ O @(T 8 > T 8 > P (T' 8) > T 16 > 4)
      # R @1 @(0-(0)-(0)///4//0//2\\4-5-4-2-0)
  Bar $ O @(T 8 > T 8 > 2)
      # R @1 @(2^4//2^4//2//2-(H*2*2))
      
