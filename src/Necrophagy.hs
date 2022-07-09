{- |
Copyright: (c) 2020 Riuga
SPDX-License-Identifier: BSD-3-Clause
Maintainer: Riuga <riuga@tuta.io>

A concise, type-level, statically-checked Haskell EDSL 
for programming guitar tablature.
-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Necrophagy ( 
  -- * Tabs
  Tablature(..),
  TabMeta(..),
  TrackList(..),
  Program(..),
  TimeStrictness(..),
  -- * Tracks
  Track(..),
  -- * Compositions
  Composition(..),
  type (/),
  -- ** Playback
  play, at, fret,
  -- ** Export
  exportMidi,
  -- * Measures
  Measure(..),
  (#),
  empty,
  -- * Poly-kinded sequences
  type (>),
  -- * Durations
  NoteDurations,
  T, T', P, P', Q, 
  -- * Tuning
  Note(..),
  Tuning,
  -- ** Common Tunings
  EStandard(..), 
  DStandard(..),
  DropAb(..),
  -- * Notes
  On,
  -- ** Combinators
  type (-),
  type (+),
  type (*),
  -- ** Dynamics
  H, M, X,
  -- ** Synonyms
  Arp, Rep,
  -- ** Modifiers
  Direction(..),
  GraceDynamic(..),
  PM, LR, Vb, Lg, Gh, Sl, Gr, Bd,
  NH, AH, SH, TH,
  Sk,
  type (^),
  type (//), type (///), type (////),
  type (\\), type (\\\), type (\\\\),
  -- *** Bend curves
  type (-@-),
  BenC, BenRelC, BenRelBenC, PreRelC,
  -- *** Custom
  ParseGraph,
  -- * Debugging
  -- ** Note values
  NoteDenomination,
  -- ** Parsing helpers
  RString,
  FStrings,
  WellFormedGraph,
  InStringRange,
  NonOverlappingChord,
  LyricsFitOutline,
  GraphFitsOutline,
  OutlineTotalsSignature,
  -- ** Fractions
  Gcd, Simplified,
  -- * Re-exports
  Nat,
  (>>), ($), (.),
  undefined,
  mempty,
  fromString,
  fromInteger,
  InstrumentName(..)
) where

import Prelude ( ($), const
               , Fractional, Double
               , String
               , Maybe(..), Bool(..), Ordering(..)
               , undefined, mempty 
               , fromIntegral, fromInteger
               , otherwise
               , realToFrac, fromRational
               , IO, putStrLn, pure
               )
import qualified Prelude
import Control.Concurrent
import Control.Arrow
import Control.Category ((.), id)
import Control.Monad hiding ((>>))
import Data.Void
import Data.Singletons hiding (Apply)
import Data.Monoid
import Data.Type.Equality
import Data.Type.Bool
import qualified Data.Text as T
import Data.String
import Data.Kind
import Euterpea hiding (PitchClass(..), Text, Marker, Tempo, play)
import qualified Euterpea as E
import GHC.TypeLits ( Nat, KnownNat, natVal
                    , Symbol, KnownSymbol, symbolVal
                    , Log2, Mod
                    , CmpNat
                    , TypeError, ErrorMessage(..)
                    )
import qualified GHC.TypeLits as Nat

-- * Tablature

-- | Greatest common denominator
type family Gcd (n :: Nat) (d :: Nat) where
  Gcd n 0 = n
  Gcd n d = Gcd d (n `Mod` d)

type family Simplified f where
  Simplified (n / d) = 
    (n `Div` Gcd n d) / (d `Div` Gcd n d)

data Tablature = Tablature TabMeta TrackList

data TabMeta
  = TabMeta { tabAuthor     :: T.Text
            , tabArtist     :: T.Text
            , tabName       :: T.Text
            , tabDifficulty :: T.Text
            , tabComments   :: T.Text
            }

data TrackList = forall v c. TrackList [Track v c]

data Program p
  = Tuned InstrumentName p

data TimeStrictness
  = Strict
  | Flexible

type family TrackTag (o :: TimeStrictness) c :: Type where
  TrackTag 'Strict   c = c
  TrackTag 'Flexible _ = Void

data Track (r :: TimeStrictness) c where
  Track :: forall r p u m m' s s' t t' c. u ~ Tuning p
        => { trackName    :: T.Text
           , trackProgram :: Program p
           , trackBody    :: Composition m m' s s' t t' u c
           } -> Track r (TrackTag r (Simplified c))

-- * Graphing

-- ** Graph unit

-- | Graph unit; (fret/dynamic) `On` (string)
data On (n :: k) (s :: l)

-- ** Dynamics

-- | Mute (Empty)
data M

-- | Dead
data X

-- | Hold
data H

-- * Combinators

-- | Sequential note combinator
data (-) (a :: k) (b :: l)
infixr 3 -

-- | Parallel arbitrary note combinator
data (+) (a :: k) (b :: l)
infixr 3 +

-- | Parallel ascending combinator (reduces in terms of (+))
data (*) (a :: k) (b :: l)
infixr 3 *

-- ** Synonyms

-- | Fret the note only if it is 'Just
type family (+?) a (b :: Maybe Type) where
  a +? ('Just b) = a + b
  a +? _ = a

infixr 3 +?

-- | Calculate notes needed to be held from previous sequences
-- in an arpeggio
type family Holds (held :: [Nat]) :: Maybe Type where
  Holds '[] = 'Nothing
  Holds (h ': hs) = ('Just (H `On` h +? Holds hs))

-- | Maybe to List
type family M2L (x :: Maybe k) :: [k] where
  M2L ('Just x) = '[x]
  M2L _ = '[]

-- | Root string (of a modifier stack)
type family RString (x :: k) :: Maybe Nat where
  RString ((x :: k) `On` s) = 'Just s
  RString ((m :: k -> Type) x) = RString x
  RString _ = 'Nothing

-- | Fretted strings
type family FStrings xs where
  FStrings (x + xs) = M2L (RString x) ::: FStrings xs
  FStrings x = M2L (RString x)

type family Arpeggiate (held :: [Nat]) a where
  --Arpeggiate hs _ (x - xs) = 
  --  Arpeggiate hs '[] x - Arpeggiate (FStrings x ::: hs) '[] xs
  Arpeggiate held (x `On` s - xs) = 
    (x `On` s +? Holds held) - Arpeggiate (s ': held) xs
  Arpeggiate held (x `On` s) = 
    x `On` s +? Holds held
  Arpeggiate _ _ = TypeError 
    ('Text "Arpeggios must use chords with explicit `On` notation.")

-- | Arpeggios
type Arp a = Arpeggiate '[] a

type family ExpandSeq (n :: Nat) a where
  ExpandSeq 1 xs = xs
  ExpandSeq n xs = xs - ExpandSeq (n `Sub` 1) xs

-- | Repeat
type Rep (n :: Nat) a
  = ExpandSeq n a


-- ** Modifiers

-- | Let ring
data LR (a :: k)

-- | Slide
data Sl (a :: k)

-- | Ghost note
data Gh (a :: k)

data GraceDynamic 
  = On 
  | Pre

-- | Grace (dynamic) (modifier) (duration) (group)
data NoteDenomination n
  => Gr (d :: GraceDynamic) m (n :: Nat) (a :: k)

-- | Bend curve vertex / (steps) -@- (time)
-- Resolution for steps is (1/4) (quarter step)
-- Resolution for time is (1/12) (relative to note duration)
data (-@-) f p

type BenC f =
  '[ (0/4) -@- (0 / 12)
   , f     -@- (6 / 12)
   ]

type BenRelC f =
  '[ (0/4) -@- (0 / 12)
   , f     -@- (3 / 12)
   , f     -@- (6 / 12)
   , (0/4) -@- (9 / 12)
   ]  

type BenRelBenC f =
  '[ (0/4) -@- (0 / 12)
   , f     -@- (2 / 12)
   , f     -@- (4 / 12)
   , (0/4) -@- (6 / 12)
   , (0/4) -@- (8 / 12)
   , f     -@- (10 / 12)
   ]

type PreRelC f =
  '[ f     -@- (0 / 12)
   , f     -@- (4 / 12)
   , (0/4) -@- (8 / 12)
   ]

-- | Bend (curve) (group)
data Bd (c :: [Type]) (a :: k)

-- | Vibrato
data Vb (a :: k)

-- | Palm mutes
data PM (a :: k)

-- | Legato (current to next note)
data Lg (a :: k)

-- | Hammer on / Pull offs, in terms of legato
type (^) (a :: k) (b :: l) = Lg a - b

infixr 6 ^

data Direction = Up | Dw

-- | String skip by @n@ in the direction @d@ before playing @a@
data Sk (d :: Direction) (n :: Nat) (a :: k)

-- | String skip +1
type (//) a b = a - Sk 'Up 1 b
-- | String skip +2
type (///) a b = a - Sk 'Up 2 b
-- | String skip +3
type (////) a b = a - Sk 'Up 3 b
-- | String skip -1
type (\\) a b = a - Sk 'Dw 1 b
-- | String skip -2
type (\\\) a b = a - Sk 'Dw 2 b
-- | String skip -3
type (\\\\) a b = a - Sk 'Dw 3 b

infixr 1 // 
infixr 1 ///
infixr 1 ////
infixr 1 \\
infixr 1 \\\
infixr 1 \\\\

-- ** Harmonics

data NH (a :: k)
data AH (a :: k)
data SH (a :: k)
data TH (a :: k)

-- * Notes

data Note (n :: Nat)
  = Ab | A | Bb | B | C | Db | D | Eb | E | F | Gb | G  

-- * Poly-kinded sequences

-- | Poly-kinded sequences
data (>) (a :: k) (b :: l)

infixr 1 >

-- * Guitar tunings

type family Tuning p = u | u -> p

data EStandard (n :: Nat) = EStandard

type instance Tuning (EStandard 6)
  = 'E @2
  > 'A @2
  > 'D @3
  > 'G @3
  > 'B @3
  > 'E @4

data DStandard (n :: Nat) = DStandard

type instance Tuning (DStandard 6)
  = 'D @2 
  > 'G @2 
  > 'C @3 
  > 'F @3 
  > 'A @3 
  > 'D @4

data DropAb (n :: Nat) = DropAb

type instance Tuning (DropAb 6)
  = 'Ab @1
  > 'Eb @2
  > 'Ab @2
  > 'Eb @3
  > 'Ab @3
  > 'Bb @3

-- * Utilities

type Add a b = a Nat.+ b
type Sub a b = a Nat.- b
type Mul a b = a Nat.* b
type Pow a b = a Nat.^ b
type Div a b = Nat.Div a b

type (>=) (a :: Nat) (b :: Nat)
  = CmpNat a b == 'GT || CmpNat a b == 'EQ

type family Length (a :: [Type]) :: Nat where
  Length '[] = 0
  Length (x ': xs) = 1 `Add` Length xs

type family Elem (x :: k) (xs :: [k]) where
  Elem x (x ': xs) = 'True
  Elem x (_ ': xs) = Elem x xs
  Elem x '[]       = 'False

type family KLength (a :: k) :: Nat where
  KLength ((x :: k) > (xs :: l)) = 1 `Add` KLength xs
  KLength (xs :: k) = 1

type family Replicate (n :: Nat) (a :: k) :: [k] where
  Replicate 0 _ = '[]
  Replicate n x = x ': Replicate (n `Sub` 1) x

-- | Replication
type Repl a b = Replicate a b

type family (:::) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ::: ys = ys
  (x ': xs) ::: ys = x ': (xs ::: ys)

type family Ensure (b :: Bool) (s :: Symbol) :: Constraint where
  Ensure 'True  _ = ()
  Ensure 'False s = TypeError ('Text s) 

-- | Type-level (<|>) specialized for 'Maybe'.
type family 
  (<|>) (a :: Maybe k)  
        (b :: Maybe k) :: Maybe k where
  (<|>) 'Nothing 'Nothing = 'Nothing
  (<|>) 'Nothing x = x
  (<|>) x 'Nothing = x
  (<|>) x _ = x

-- | Type-level 'Just unwrap for 'Maybe'
type family Unwrap (a :: Maybe k) :: k where
  Unwrap ('Just x) = x
  Unwrap _ = TypeError ('Text "Unwrapped 'Nothing")

-- * Composition

-- | Time signatures (quarters/min) | Fractionals
data (/) (n :: Nat) (d :: Nat)

-- | Fraction addition
type family (^+^) f f' where
  (b / v) ^+^ (b' / v') = 
    ((b `Mul` v') `Add` (b' `Mul` v)) / (v `Mul` v')

-- | Fraction summation
type family FractionalSum (xs :: [Type]) where
  FractionalSum '[] = (0 / 1)
  FractionalSum (x ': xs) = x ^+^ FractionalSum xs

-- | Composition
-- @m@ - section marker
-- @s@ - time signature
-- @t@ - tempo
-- @u@ - tuning
-- @c@ - cumulative time
data Composition (m :: Symbol) (m' :: Symbol) 
                  s             s' 
                 (t :: Nat)    (t' :: Nat) u c where
  Marker
    :: forall (m' :: Symbol) m s t u. KnownSymbol m'
    => Composition m m' s s t t u (0 / 1)
  Tempo 
    :: forall (t' :: Nat) t m s u. KnownNat t'
    => Composition m m s s t t' u (0 / 1)
  Sig
    :: forall s' s (n :: Nat) (d :: Nat) m (t :: Nat) u.
        ( Fraction s' n d
        , NoteDenomination d
        )
    => Composition m m s s' t t u (0 / 1)
  Bar 
    :: forall m (n :: Nat) (d :: Nat) (t :: Nat) s u g o l. 
        ( Fraction s n d
        , OutlineTotalsSignature s o
        , WellFormedGraph (KLength u) g
        , GraphFitsOutline o g
        , LyricsFitOutline o l 
        , Musicalize u o g
        )
    => Measure o g l
    -> Composition m m s s t t u ((n `Mul` t) / (d `Mul` 4))
  Compose 
    :: forall u m m' s s' t t' c c' j r g. ()
    => Composition m m' s s' t t' u c
    -> Composition m' g s' r t' j u c'
    -> Composition m g s r t j u (c ^+^ c')

-- * Durations

type NoteDenomination (n :: Nat) = (2 `Pow` Log2 n) ~ n

-- | Triplets
data T (v :: Nat)
-- | Triplets (standalone)
data T' (v :: Nat)
-- | Quadruplets
data Q (v :: Nat)
-- | Pointed (single dotted) notes
data P v
-- | Extra-pointed (double dotted) notes
data P' v

type family ExpandDotted (dots :: Nat) (n :: Nat) (d :: Nat) where
  ExpandDotted 0    n d = (n / d)
  ExpandDotted dots n d = 
    (n / ((2 `Pow` dots) `Mul` d)) ^+^ ExpandDotted (dots `Sub` 1) n d

type family NoteDurations (v :: k) = (a :: [Type])

type instance NoteDurations (u > v) 
  = NoteDurations u ::: NoteDurations v

type instance NoteDurations (v :: Nat)     
  = '[1 / v]
type instance NoteDurations (T (v :: Nat)) 
  = Repl 3 (2 / (3 `Mul` v))
type instance NoteDurations (T' (v :: Nat)) 
  = '[2 / (3 `Mul` v)]
type instance NoteDurations (Q (v :: Nat)) 
  = Repl 4 (1 / v)
type instance NoteDurations (P (v :: Nat)) 
  = '[ExpandDotted 1 1 v]
type instance NoteDurations (P (T' v))
  = '[ExpandDotted 1 2 (3 `Mul` v)]
type instance NoteDurations (P' (v :: Nat)) 
  = '[ExpandDotted 2 1 v]
type instance NoteDurations (P' (T' v))
  = '[ExpandDotted 2 2 (3 `Mul` v)]

-- | Measure 
--
-- @o@ - parsed durations
-- @g@ - parsed graph
-- @g@ - parsed lyrics
data Measure o g (l :: Maybe [Symbol]) where
  -- | Outline (note values)
  O 
    :: forall v g. ()
    => Measure (NoteDurations v) g 'Nothing
  -- | Run, @r@ - root (starting) string, @g@ - run graph
  R 
    :: forall r g (o :: [Type]). ()
    => Measure o (ParseGraph r g) 'Nothing
  -- | Lyrics, @l@ - lyric sequence
  L
    :: forall l o g. ()
    => Measure o g ('Just (ParseLyrics l))
  -- | Combinator
  Splice 
    :: forall o g l l' h. (h ~ (l <|> l'))
    => Measure o g l
    -> Measure o g l'
    -> Measure o g h

(#) = Splice

-- | An empty measure
empty :: Measure '[(1/1)] '[M `On` 1] 'Nothing
empty = O @1 # R @1 @M

type family Apply (f :: k -> l) (xs :: [k]) :: [l] where
  Apply f '[] = '[]
  Apply f (x ': xs) = f x ': Apply f xs

type family ApplyBend (c :: [Type]) (xs :: [k]) :: [l] where
  ApplyBend c '[] = '[]
  ApplyBend c (x ': xs) = Bd c x ': ApplyBend c xs

type family ApplyGrace (d :: GraceDynamic) m (n :: Nat) (a :: [k]) where
  ApplyGrace d m n '[] = '[]
  ApplyGrace d m n (x ': xs) = Gr d m n x ': ApplyGrace d m n xs

type family Expand (n :: Nat) (xs :: [k]) :: [k] where
  Expand 0 _  = '[]
  Expand n xs = xs ::: Expand (n `Sub` 1) xs

type OutlineTotalsSignature s o
  = Ensure (Simplified (FractionalSum o) == Simplified s)
      "Note value outline does not total time signature."

-- | Produces a proof if parsed graph @g@ is well-formed.
type family WellFormedGraph (t :: Nat) (g :: k) :: Constraint where
  WellFormedGraph _ '[] = ()
  -- Sequential test
  WellFormedGraph t (x ': xs) = 
    ( WellFormedGraph t x
    , WellFormedGraph t xs
    )
  -- Parallel test
  WellFormedGraph t (x + xs) =
    ( WellFormedGraph t x
    , WellFormedGraph t xs
    , NonOverlappingChord (Unwrap (RString x)) (FStrings xs)
    )
  -- Modifier stack
  WellFormedGraph t x = InStringRange t (Unwrap (RString x))

type NonOverlappingChord (x :: Nat) (xs :: [Nat])
  = Ensure (Not (x `Elem` xs))
      "There can only be one fretted note per string in a chord."

type InStringRange (t :: Nat) (r :: Nat)
  = Ensure (t >= r)
      "String range out-of-bounds in graph."

type GraphFitsOutline d g'
  = Ensure (Length g' == Length d)
      "Number of notes in a run must match number of \
      \declared note values."

type family LyricsFitOutline d l where
  LyricsFitOutline d ('Just l) =
    Ensure (Length d >= KLength l)
      "Number of lyric syllables must match note outline."
  LyricsFitOutline _ _ = ()

type family ParseLyrics (l :: k) :: [Symbol] where
  ParseLyrics ((l :: Symbol) > ls) = l ': ParseLyrics ls
  ParseLyrics (l :: Symbol) = '[l]

-- | Flatten note modifiers down to a linear list of notes with
-- stacks of modifiers.
--
-- @r@ - current string
-- @g@ - graph
type family ParseGraph (r :: Nat) (g :: k) :: [Type]

-- ** Unit instances

-- | Mute (empty) note 
type instance ParseGraph r M
  = '[M `On` r]
-- | Dead note
type instance ParseGraph r X
  = '[X `On` r]
-- | Held note
type instance ParseGraph r H
  = '[H `On` r]
-- | Pluck (single note)
type instance ParseGraph r (n :: Nat) 
  = '[n `On` r]
-- | Pluck (single note) (explicit string)
type instance ParseGraph r (n `On` s)
  = '[n `On` s]


-- ** Combinator instances

-- | Sequence notes
type instance ParseGraph r (xs - ys)
  = ParseGraph r xs ::: ParseGraph r ys
-- | Skip up (amount) (group)
type instance ParseGraph r (Sk 'Up n xs) 
  = ParseGraph (r `Add` n) xs
-- | Skip down (amount) (group)
type instance ParseGraph r (Sk 'Dw n xs)
  = ParseGraph (r `Sub` n) xs

-- * Chord instances

type family Head (xs :: [k]) where
  Head (x ': _) = x
  Head _ = TypeError ('Text "Matching on Head failed.")

type instance ParseGraph r (x * xs)
  = '[Head (ParseGraph r x) + Head (ParseGraph (r `Add` 1) xs)]

type instance ParseGraph r ((x `On` s) + xs)
  = '[(x `On` s) + Head (ParseGraph r xs)]

-- ** Stock modifier instances

-- | Slide
type instance ParseGraph r (Sl xs)
  = Apply Sl (ParseGraph r xs)
-- | Legato (group)
type instance ParseGraph r (Lg xs) 
  = Apply Lg (ParseGraph r xs)
-- | Ghost note
type instance ParseGraph r (Gh xs)
  = Apply Gh (ParseGraph r xs)
-- | Palm mute (group)
type instance ParseGraph r (PM xs) 
  = Apply PM (ParseGraph r xs)
-- | Vibrato (group)
type instance ParseGraph r (Vb xs) 
  = Apply Vb (ParseGraph r xs)
-- | Let ring (group)
type instance ParseGraph r (LR xs) 
  = Apply LR (ParseGraph r xs)
-- | Natural harmonic
type instance ParseGraph r (NH xs)
  = Apply NH (ParseGraph r xs)
-- | Artificial harmonic
type instance ParseGraph r (AH xs)
  = Apply AH (ParseGraph r xs)
-- | Semi-harmonic
type instance ParseGraph r (SH xs)
  = Apply SH (ParseGraph r xs)
-- | Tap harmonic
type instance ParseGraph r (TH xs)
  = Apply TH (ParseGraph r xs)
-- | Bend 
type instance ParseGraph r (Bd c xs)
  = ApplyBend c (ParseGraph r xs)
-- | Grace
type instance ParseGraph r (Gr d m n xs)
  = ApplyGrace d m n (ParseGraph r xs)

(>>) = Compose
infixr 1 >>

-- * Music conversion

div :: Fractional a => a -> a -> a
div = (Prelude./)

duration :: forall f n d. Fraction f n d => Proxy f -> Dur
duration _ = realToFrac (natVal (Proxy @n)) `div` realToFrac (natVal (Proxy @d))

type family NoteIndex (n :: Nat) (xs :: k) :: SomeNote where
  NoteIndex 1 ((x :: Note _) > _) = 'SomeNote x
  NoteIndex 1 (x :: Note _) = 'SomeNote x
  NoteIndex n (_ > xs) = NoteIndex (n `Sub` 1) xs

data SomeNote = forall (o :: Nat). SomeNote (Note o)

type family HalfStepsFrom (f :: Nat) (n :: SomeNote) :: SomeNote where
  0 `HalfStepsFrom`  n                          = n
  n `HalfStepsFrom` ('SomeNote ('Ab :: Note o)) = (n `Sub` 1) `HalfStepsFrom` 'SomeNote ('A  @o)
  n `HalfStepsFrom` ('SomeNote ('A  :: Note o)) = (n `Sub` 1) `HalfStepsFrom` 'SomeNote ('Bb @o)
  n `HalfStepsFrom` ('SomeNote ('Bb :: Note o)) = (n `Sub` 1) `HalfStepsFrom` 'SomeNote ('B  @o)
  n `HalfStepsFrom` ('SomeNote ('B  :: Note o)) = (n `Sub` 1) `HalfStepsFrom` 'SomeNote ('C  @(o `Add` 1))
  n `HalfStepsFrom` ('SomeNote ('C  :: Note o)) = (n `Sub` 1) `HalfStepsFrom` 'SomeNote ('Db @o)
  n `HalfStepsFrom` ('SomeNote ('Db :: Note o)) = (n `Sub` 1) `HalfStepsFrom` 'SomeNote ('D  @o)
  n `HalfStepsFrom` ('SomeNote ('D  :: Note o)) = (n `Sub` 1) `HalfStepsFrom` 'SomeNote ('Eb @o)
  n `HalfStepsFrom` ('SomeNote ('Eb :: Note o)) = (n `Sub` 1) `HalfStepsFrom` 'SomeNote ('E  @o)
  n `HalfStepsFrom` ('SomeNote ('E  :: Note o)) = (n `Sub` 1) `HalfStepsFrom` 'SomeNote ('F  @o)
  n `HalfStepsFrom` ('SomeNote ('F  :: Note o)) = (n `Sub` 1) `HalfStepsFrom` 'SomeNote ('Gb @o)
  n `HalfStepsFrom` ('SomeNote ('Gb :: Note o)) = (n `Sub` 1) `HalfStepsFrom` 'SomeNote ('G  @o)
  n `HalfStepsFrom` ('SomeNote ('G  :: Note o)) = (n `Sub` 1) `HalfStepsFrom` 'SomeNote ('Ab @o)

type LookupNote u (f :: Nat) (s :: Nat) 
  = f `HalfStepsFrom` NoteIndex s u

class ToPitch (n :: SomeNote) where
  toPitch :: Proxy n -> Pitch

instance KnownNat o => ToPitch ('SomeNote ('Ab :: Note o)) where 
  toPitch _ = (E.Af, fromIntegral $ natVal (Proxy @o))
instance KnownNat o => ToPitch ('SomeNote ('A  :: Note o)) where 
  toPitch _ = (E.A,  fromIntegral $ natVal (Proxy @o))
instance KnownNat o => ToPitch ('SomeNote ('Bb :: Note o)) where 
  toPitch _ = (E.Bf, fromIntegral $ natVal (Proxy @o))
instance KnownNat o => ToPitch ('SomeNote ('B  :: Note o)) where 
  toPitch _ = (E.B,  fromIntegral $ natVal (Proxy @o))
instance KnownNat o => ToPitch ('SomeNote ('C  :: Note o)) where 
  toPitch _ = (E.C,  fromIntegral $ natVal (Proxy @o))
instance KnownNat o => ToPitch ('SomeNote ('Db :: Note o)) where 
  toPitch _ = (E.Df, fromIntegral $ natVal (Proxy @o))
instance KnownNat o => ToPitch ('SomeNote ('D  :: Note o)) where 
  toPitch _ = (E.D,  fromIntegral $ natVal (Proxy @o))
instance KnownNat o => ToPitch ('SomeNote ('Eb :: Note o)) where 
  toPitch _ = (E.Ef, fromIntegral $ natVal (Proxy @o))
instance KnownNat o => ToPitch ('SomeNote ('E  :: Note o)) where 
  toPitch _ = (E.E,  fromIntegral $ natVal (Proxy @o))
instance KnownNat o => ToPitch ('SomeNote ('F  :: Note o)) where 
  toPitch _ = (E.F,  fromIntegral $ natVal (Proxy @o))
instance KnownNat o => ToPitch ('SomeNote ('Gb :: Note o)) where 
  toPitch _ = (E.Gf, fromIntegral $ natVal (Proxy @o))
instance KnownNat o => ToPitch ('SomeNote ('G  :: Note o)) where 
  toPitch _ = (E.G, fromIntegral $ natVal (Proxy @o))

type family FractionList n d (ts :: [Type]) :: Constraint where
  FractionList n d '[] = ()
  FractionList n d (x ': _) = x ~ (n / d)

type Fraction t n d
  = ( t ~ (n / d)
    , KnownNat n
    , KnownNat d
    )

exportMidi :: String -> Composition m m' s s' t t' u c -> IO ()
exportMidi file = writeMidi file . Modify (E.Instrument OverdrivenGuitar) . line . composeSlices

play :: Composition m m' s s' t t' u c -> IO ()
play = playDev 2 . Modify (E.Instrument OverdrivenGuitar) . line . composeSlices

at :: String -> Composition m m' s s' t t' u c -> IO ()
at marker (x `Compose` xs)
  | sameMarker marker x = playDev 2 $ Modify (E.Tempo 1.75) $ line $ composeSlices xs
  | otherwise = at marker xs
at marker m@Marker = when (sameMarker marker m) $ putStrLn "Empty section."
at _ _ = putStrLn "Marker not found."

fret 
  :: forall position tuningId tuning note fret string. 
      ( position ~ fret `On` string
      , tuning ~ Tuning tuningId
      , note ~ LookupNote tuning fret string
      , ToPitch note
      ) 
  => IO ()
fret = void . forkIO . playDev 2 . Modify (E.Instrument OverdrivenGuitar) . line . pure $ musicalize (Proxy @tuning) (Proxy @'[(1/2)]) (Proxy @'[(fret `On` string)])

sameMarker 
  :: forall m m' s s' t t' u c. ()
  => String
  -> Composition m m' s s' t t' u c 
  -> Bool
sameMarker marker Marker = marker Prelude.== symbolVal (Proxy @m')
sameMarker _ _ = False

composeSlices :: forall m m' s s' t t' u c. Composition m m' s s' t t' u c -> [Music Pitch]
composeSlices (x `Compose` xs) = go x xs
  where
    go :: forall q w e j l. ()
       => Composition m q s w t e u j 
       -> Composition q m' w s' e t' u l 
       -> [Music Pitch]
    go Tempo rs = pure . Modify (E.Tempo . (`div` 120) . fromIntegral . natVal $ Proxy @e) $ line (composeSlices rs)
    go l     rs = composeSlices l <> composeSlices rs
composeSlices (Bar (_ :: Measure o g l)) = [musicalize (Proxy @u) (Proxy @o) (Proxy @g)]
composeSlices _ = [Prim (Rest 0)]

class Musicalize u (ts :: [Type]) (vs :: [Type]) where
  musicalize :: Proxy u -> Proxy ts -> Proxy vs -> Music Pitch

instance Musicalize u '[] '[] where
  musicalize _ _ _ = Prim (Rest 0)

instance ( GraphUnit u t v
         , Musicalize u ts vs
         , Fraction t n d 
         ) => Musicalize u (t ': ts) (v ': vs) where
  musicalize _ _ _  =  graphUnit  (Proxy @u) (Proxy @t)  (Proxy @v) 
                   :+: musicalize (Proxy @u) (Proxy @ts) (Proxy @vs)

-- | Parsing of graph units, i.e. @f `On` s@ elements.
class GraphUnit u t f where
  graphUnit :: Proxy u -> Proxy t -> Proxy f -> Music Pitch

instance ( ToPitch l
         , l ~ LookupNote u f s
         , Fraction t n d
         ) => GraphUnit u t ((f :: Nat) `On` (s :: Nat)) where
  graphUnit _ _ _ = note (duration (Proxy @t)) . toPitch $ Proxy @l

instance {-# OVERLAPPING #-} Fraction t n d => GraphUnit u t (H `On` s) where
  graphUnit _ _ _ = rest . duration $ Proxy @(0/1)

instance {-# OVERLAPPING #-} Fraction t n d => GraphUnit u t (M `On` s) where
  graphUnit _ _ _ = rest . duration $ Proxy @t

instance {-# OVERLAPPING #-} Fraction t n d => GraphUnit u t (X `On` s) where
  graphUnit _ _ _ = rest . duration $ Proxy @t

instance {-# OVERLAPPABLE #-} GraphUnit u t s => GraphUnit u t ((m :: k -> Type) s) where
  graphUnit _ _ _ = graphUnit (Proxy @u) (Proxy @t) (Proxy @s)

instance {-# OVERLAPS #-} (GraphUnit u t x, GraphUnit u t xs) => GraphUnit u t (x + xs) where
  graphUnit _ _ _ =  graphUnit (Proxy @u) (Proxy @t) (Proxy @x) 
                 :=: graphUnit (Proxy @u) (Proxy @t) (Proxy @xs)

