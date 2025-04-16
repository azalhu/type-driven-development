module Exercises

%default total

data GuessCmd : Type -> Nat -> Nat -> Type where
  Try : Integer -> GuessCmd Ordering (S state) state
  -------------
  Pure : ty -> GuessCmd ty state state
  (>>=) : GuessCmd a state1 state2 ->
          (a -> GuessCmd b state2 state3) ->
          GuessCmd b state1 state3
  (>>) : GuessCmd () state1 state2 ->
         Lazy (GuessCmd b state2 state3) ->
         GuessCmd b state1 state3

threeGuesses : GuessCmd () 3 0
threeGuesses = do
  _ <- Try 10
  _ <- Try 20
  _ <- Try 15
  Pure ()

--noGuesses : GuessCmd () 0 0
--noGuesses = do
--  _ <- Try 10
--  Pure ()

data Matter = Solid | Liquid | Gas

data MatterCmd : Type -> Matter -> Matter -> Type where
  Melt : MatterCmd () Solid Liquid
  Boil : MatterCmd () Liquid Gas
  Condense : MatterCmd () Gas Liquid
  Freeze : MatterCmd () Liquid Solid
  ------
  Seq : MatterCmd () state1 state2 ->
        Lazy (MatterCmd () state2 state3) ->
        MatterCmd () state1 state3

namespace MatterDo
  export
  (>>) : MatterCmd () state1 state2 ->
         Lazy (MatterCmd () state2 state3) ->
         MatterCmd () state1 state3
  (>>) = Seq

iceSteam : MatterCmd () Solid Gas
iceSteam = do
  Melt
  Boil

steamIce : MatterCmd () Gas Solid
steamIce = do
  Condense
  Freeze

--overMelt : MatterCmd () Solid Gas
--overMelt = do
--  Melt
--  Melt

