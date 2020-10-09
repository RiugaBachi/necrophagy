### necrophagy
1. (n)	The eating of dead or decaying animal flesh
2. (n)  Feeding on corpses or carrion
3. (n)  The art of tabbing out statically- type-checked technical death metal

![](https://i.imgur.com/9GbqXzx.png)

# necrophagy
[![GitHub CI](https://github.com/riugabachi/necrophagy/workflows/CI/badge.svg)](https://github.com/riugabachi/necrophagy/actions)
[![Hackage](https://img.shields.io/hackage/v/necrophagy.svg?logo=haskell)](https://hackage.haskell.org/package/necrophagy)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

A type-level, statically-verified Haskell embedded domain specific language (EDSL) for writing guitar tablature.

Named in honor of Necrophagist, the pioneers of modern technical death metal. The music is already technical, what's a little bit of hardcore type-level programming to phase us?

Currently, this library only provides the type-level facilities needed to describe guitar tablature spanning multiple tracks at the type level. Future goals include GP5, MIDI, and WAV backends to make these tabs more useful beyond direct reading.

Transcription workflow is heavily designed around GHCi. Once MIDI / GP5 support is implemented, workflow-related functions for delimited section playback and tab file generation will be added with the intent that they be invoked from GHCi sessions interpreting individual tablature source files.

## Try it out

```bash
>> cabal update
>> cabal install --lib necrophagy
```
Check out the included example tabs to see if they typecheck; maybe even play around with the kinds and types of the various tracks:

```bash
>> cabal repl necrophagy-examples
```
If you like what you see, you may want to try writing your own tabs based off of one of the example source files. It is as simple as:

```bash
>> touch MyTab.hs
```
---
```hs
{-# LANGUAGE 
  NoMonoLocalBinds, RebindableSyntax, DataKinds,
  OverloadedStrings, TypeOperators, TypeApplications,
  PartialTypeSignatures, NoStarIsType
#-}

module Tab where

import Necrophagy

myTab :: Tablature
myTab = Tablature tabMeta tracks
  where
    tabMeta = TabMeta
      { tabAuthor = ""
      , tabArtist = ""
      , tabName   = ""
      , tabDifficulty = ""
      , tabComments   = mempty
      }
    
    -- Change this to @Strict if you want equivalence between
    -- the composition durations of all tabs enforced. 
    -- Useful when dealing with polyrhythms.
    tracks = TrackList @Flexible
      [ Track 
          { trackName    = "Player - Instrument"
          , trackProgram = DistortitionGuitar `Tuned` EStandard6
          , trackBody    = myTrack
          }
      ]

myTrack :: _
myTrack = do
  Tempo @240
  Sig @(4/4)
  -- ...And tab away!
```
---
```hs
>> ghci MyTab.hs
Tab> :t myTrack
Tab> myTab 
```

## Documentation

Every track body is a `Composition m m' s s' t t' u c`. Let us break the type parameters down:

- `m` is initial (last specified) section marker.
- `m'` is the final (new) section marker.
- `s` is the initial (last specified) time signature.
- `s'` is the final (new) time signature.
- `t` is the initial (last specified) tempo.
- `t'` is the final (new) tempo.
- `u` is the tuning, represented by a poly-kinded list of notes e.g. `(E > A > D > G > B > E)`
- `c` is the duration of the composition, also called 'cumulative time'. This is used for static equivalence enforcement should the `TrackList` be parameterized over `@Strict`.

A `Composition` has four commands:

- `Marker @m`, where `m` is a `Symbol` (type-level string)
  - `(s ~ s', t ~ t')`, since neither signature nor tempo are changed.
- `Tempo @n`, specified in quarter beats per minute.
  - `(m ~ m', s ~ s')`, since neither signature nor marker are changed.
- `Sig @(n/d)`, where `d` is enforced as a power of 2.
  - `(m ~ m', t ~ t')`, since neither tempo nor marker are changed.
- `Bar $ measure`, where measure is a `Measure`.
  - `(m ~ m', t ~ t', s ~ s')`, since neither tempo nor signature nor marker are changed.

Each measure is a `Measure o g l`. The type parameters are:

- `o` - the parsed sequence of note values (outline of durations)
- `g` - the parsed graph of notes
- `l` - the parsed sequence of lyrics, if any

A `Measure` has three commands:

- `O @o` - **O**utline of note values `o`, a poly-kinded list of enforced power-of-2 naturals `n` each representing a beat worth `1/n`. These can be wrapped in the `P n` and `P' n` type constructors for dotted and double-dotted (think **P** as in **pointed**) notes respectively. There are also "group modifiers" such as `Q n` for quadruplets, `T n` for triplets, which will expand to the appropriate amount of note values, each with the correct duration.
- `R @s @g` - **R**un of notes starting at string `s` and specified by note graph `g`. Strings can be skipped up and down via the `//` `///` `////` `\\` `\\\` `\\\\` combinators respectively, each denoting a skip of `(number of slashes - 1)` strings. Larger skips will require the `Sk Up n g` or `Sk Dw n g` modifiers, which slash notation is defined in terms of internally.
- `L @l` - **L**yric sequence denoted by `l`, a polykinded sequence of `Symbol`s. Must be less than or equal to the number of declared note values in the outline.

***Measures commands are composed via the splicing*** `(#)` ***operator.***

### Notes

A parsed note is represented as ``m (f `On` s)``, with `f` denoting the fret number and `s` the string number. `m` represents the modifier stack, which is the stack of dynamics that are applied to the particular note. For example, ``Vr (AH (5 `On` 3))`` represents a pinch (artificial) harmonic with vibrato on fret 5 of string 3. In this case, the modifier stack `m` refers to `Vr (AH _)`.

Notes are composed via the combinators `(-)`, `(+)`, and `(*)`. There are four basic notes:

1. `n :: Nat`, any type-level natural representing a single fret.
2. `H`, a hold/sustain on the previous declared note on the same string.
3. `M`, a mute/silent/empty note; the lack of a note, in other words.
4. `X`, a dead note.

A sequence of notes is composed via `(-)`, and additionally when inside an arbitrary pair of parentheses, marks a **note group** `g`. Note groups can be conveniently replicated via the `Rep n g` type family. Additionally, a variety of dynamics (effects) can be applied to each note group `g`. Dynamics can be stacked, internally referred to as a "modifier stack".

Chords are composed via `(+)` or `(*)`. `(*)` is what you will normally use, and it reduces down in terms of `(+)` internally. `(+)` can only represent modifier stacks that contain an ``f `On` s`` type at its root. In other words, `(+)` requires explicitly-annotated strings on both sides. This is useful for representing small, discontinuous chords. `(*)`, on the other hand, implicitly ascends one string at a time, which is useful for representing continuous chords.

### Synonyms

Synonyms expand to note groups, potentially with chords in them. These are useful for concisely describing common measure structures. Two stock synonyms are provided:

- `Arp g`, arpeggiates the group `g`. This means each note or chord in the sequence is converted into a new chord with the correct ``H `On` s`` notes to sustain prior notes in the sequence.
- `Rep n g`, repeats the group `g` `n` times.

### Dynamics

- `n ^ n'`, applies legato (hammer-on/pull-off) from fret `n` to fret `n'`. Isomorphic to `n - Lg n'`.
- `PM g`, applies palm mutes to all notes in `g`.
- `LR g`, applies let-ring to all notes in `g`.
- `Vb g`, applies vibrato to all notes in `g`.
- `Gh g`, applies a ghost note effect to all notes in `g`.
- `Gr d t n g`, applies a grace note effect with grace dynamic `d` (`'On` or `'Pre`), duration `t` (note value/power of two), and initial fret `n` to all notes in `g`.
- `Sl g`, applies sliding to all notes in `g`.
- `Bd c s`, applies a bend with curve `c` to all notes in `g`.
  - More on this below.
- `NHarm g`, `AHarm g`, `SHarm g`, `THarm g` applies natural/artificial/semi-/tap harmonics to all notes in `g`.

#### Bend Curves

Bend curves are denoted by a type-level list of bend curve vertices `(-@-)`. Each bend curve vertex has the form `s -@- t`, where `s` is a fraction representing how many steps (tones) the string is bent at some particular point in time `t`, which is in turn also a fraction, but with a maximum value of `(1/1)`, relative to the total duration of the note. As an example, the bend curve `BenRelC f` is defined as follows:

```hs
type BenRelC f =
  '[ (0/4) -@- (0 / 12)
   , f     -@- (3 / 12)
   , f     -@- (6 / 12)
   , (0/4) -@- (9 / 12)
   ]  
```

Four stock bend curves are provided for common use cases:

- `BenC f` - a regular bend
- `BenRelC f` - bend then release
- `BenRelBenC f` - bend, release, and bend again
- `PreRelC f` - prebend and release

The parameter `f` denotes a fraction standing in for the maximum `s` value of the vertices in the curve.

## Credits 

The core of this library was largely written over the course of a single day with some minor follow-up additions in terms of dynamics modifiers, better type errors, et cetera. In closing I would like to pay homage to Necrophagist, '92 - '09, for supplying me with some sick nostalgic tunes to work against while getting this giant type tetris puzzle just right. I suppose nothing would be more appropriate to conclude this readme with than a recent fan re-recording of an unreleased Necrophagist song, only ever played live just a few months before the band became inactive (clickable link!).

<center>

[![](http://img.youtube.com/vi/Gkh33uVlST0/0.jpg)](http://www.youtube.com/watch?v=Gkh33uVlST0 "Necrophagist - Dawn and Demise (Covered by Peripheral Cortex)")

</center>
