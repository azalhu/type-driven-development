module QTT

import Data.Vect

%default total

---------- Chapter 3: Quantities in Types

namespace Ch3_Examples
  id_explicit : (0 a : Type) -> a -> a
  id_explicit a x = x
  --id_explicit a x = ?id_explicit_rhs

  append : Vect n a -> Vect m a -> Vect (n + m) a
  append = (++)
  --append = ?append_rhs
  
  length' : {n : Nat} -> Vect n a -> Nat
  length' xs = n
  --length' xs = ?length'_rhs

  length'' : Vect n a -> Nat
  length'' [] = Z
  length'' (_ :: xs) = S (length'' xs)
  --length'' (_ :: xs) = ?length''_rhs

namespace Ch3_RunLengthEncoding
  export
  rep : Nat -> a -> List a
  rep Z x = []
  rep (S k) x = x :: rep k x

  public export
  data RunLength : List ty -> Type where
    Empty : RunLength []
    Run : (n : Nat) -> (x : ty) -> (rle : RunLength more) ->
          RunLength (rep (S n) x ++ more)

  data Singleton : a -> Type where
    Val : (x : a) -> Singleton x

  export
  uncompress : RunLength xs -> Singleton xs
  uncompress Empty = Val []
  uncompress (Run n x y) = let Val ys = uncompress y in
                               Val (x :: (rep n x ++ ys))
                              -- ?rhs
  --uncompress rle = ?uncompress_rhs

namespace Ch3_Linearity
  --data MyPair : Type -> Type -> Type where
  --  MkMyPair : (1 x : a) -> (1 y : b) -> MyPair a b

  --dup : (1 x : a) -> MyPair a a
  --dup x = MkMyPair x ?second_x

  --insert : a -> (1 xs : List a) -> (0 _ : Ordered xs) -> List a

  data MyIORes : Type -> Type where
    MkMyIORes : a -> (1 w : %World) -> MyIORes a

  MyPrimIO : Type -> Type
  MyPrimIO a = (1 x : %World) -> MyIORes a

  data MyIO : Type -> Type where
    MkMyIO : (1 fn : MyPrimIO a) -> MyIO a

  my_io_bind : (1 act : MyIO a) -> (1 k : a -> MyIO b) -> MyIO b
  my_io_bind (MkMyIO fn) = \k => MkMyIO (\w => let MkMyIORes x' w' = fn w
                                                   MkMyIO res = k x' in res w')

---------- Chapter 4: Linear Resource Usage Protocols

data Usage = None | Linear | Unrestricted

fromInteger : (x : Integer) -> {auto _ : Either (x = 0) (x = 1)} -> Usage
fromInteger 0 = None
fromInteger 1 = Linear
fromInteger _ = Unrestricted

--0 ContType : Usage -> Usage -> Type -> Type -> Type
--
-- see library Control.Linear.LIO
--data L : {default Unrestricted use : Usage} ->
--         (ty : Type) -> Type where
--  [search ty]
--  Pure0 : (0 _ : a) -> L {use=0} a
--  Pure1 : (1 _ : a) -> L {use=1} a
--  PureW : a -> L a
--  ----------
--  Action : (1 _ : a) -> L {use} a
--  Bind : {u_act : _} ->
--         (1 _ : L {use=u_act} a) ->
--         (1 _ : ContType u_act u_k a b) ->
--         L {use=u_k} b
--
--L0 : (ty : Type) -> Type
--L0 ty = L {use=0} ty
--
--L1 : (ty : Type) -> Type
--L1 ty = L {use=1} ty
--
--namespace LBind
--  public export
--  (>>=) : {u_act : _} ->
--          (1 _ : L {use=u_act} a) ->
--          (1 _ : ContType u_act u_k a b) ->
--          L {use=u_k} b
--  (>>=) = Bind
--
--ContType None u_k a b = (0 _ : a) -> L {use=u_k} b
--ContType Linear u_k a b = (1 _ : a) -> L {use=u_k} b
--ContType Unrestricted u_k a b = a -> L {use=u_k} b
--
--RunCont : Usage -> Type -> Type -> Type
--RunCont None t b = (0 _ : t) -> b
--RunCont Linear t b = (1 _ : t) -> b
--RunCont Unrestricted t b = t -> b

data L : {default Unrestricted use : Usage} -> Type -> Type where

pure : (x : a) -> L a
pure0 : (0 x : a) -> L {use=0} a
pure1 : (1 x : a) -> L {use=1} a

ContType : (u_a : Usage) -> (u_k : Usage) -> Type -> Type -> Type
ContType None u_k a b = (0 _ : a) -> L {use=u_k} b
ContType Linear u_k a b = (1 _ : a) -> L {use=u_k} b
ContType Unrestricted u_k a b = a -> L {use=u_k} b

(>>=) : {u_act : _} ->
        (1 _ : L {use=u_act} a) ->
        (1 _ : ContType u_act u_k a b) ->
        L {use=u_k} b

action : IO a -> L a

data ATMState = Ready | CardInserted | Session
data ATM : ATMState -> Type

initATM : L {use=1} (ATM Ready)
shutDown : (1 _ : ATM Ready) -> L ()

||| Predicate on ATMState.
data HasCard : ATMState -> Type where
  HasCardPINNotChecked : HasCard CardInserted
  HasCardPINChecked : HasCard Session

data PINCheck = CorrectPIN | IncorrectPIN

insertCard : (1 _ : ATM Ready) -> L {use=1} (ATM CardInserted)
checkPIN : (1 _ : ATM CardInserted) -> (pin : Int) ->
           L {use=1}
             (Res PINCheck
                  (\res => ATM (case res of
                                     CorrectPIN => Session
                                     IncorrectPIN => CardInserted)))
dispense : (1 _ : ATM Session) -> L {use=1} (ATM Session)
getInput : HasCard st => (1 _ : ATM st) ->
                         L {use=1} (Res String (const (ATM st)))
ejectCard : HasCard st => (1 _ : ATM st) -> L {use=1} (ATM Ready)
message : (1 _ : ATM st) -> String -> L {use=1} (ATM st)

runATM : L ()
runATM = do
  m <- initATM
  m <- insertCard m
  CorrectPIN # m <- checkPIN m 1234
  | IncorrectPIN # m => do
    m <- ejectCard m
    shutDown m
  m <- dispense m
  m <- ejectCard m
  shutDown m

------------ Chapter 5: Session Types via QTT

data Actions : Type where
  Send : (a : Type) -> (a -> Actions) -> Actions
  Recv : (a : Type) -> (a -> Actions) -> Actions
  Close : Actions

data Channel : Actions -> Type

data Protocol : Type -> Type where
  Request : (a : Type) -> Protocol a
  Respond : (a : Type) -> Protocol a
  ----------
  ProtocolDo : Protocol a -> (a -> Protocol b) -> Protocol b
  ProtocolSeq : Protocol () -> Protocol b -> Protocol b
  Done : Protocol ()

namespace ProtocolBind
  export
  (>>=) : Protocol a -> (a -> Protocol b) -> Protocol b
  (>>=) = ProtocolDo

  export
  (>>) : Protocol () -> Protocol b -> Protocol b
  (>>) = ProtocolSeq

data Command = Add | Reverse

AsClient, AsServer : Protocol a -> Actions

Client, Server : Protocol a -> Type
Client p = Channel (AsClient p)
Server p = Channel (AsServer p)

send : (1 chan : Channel (Send ty next)) -> (val : ty) ->
       L {use=1} (Channel (next val))
recv : (1 chan : Channel (Recv ty next)) ->
       L {use=1} (Res ty (\val => Channel (next val)))
close : (1 chan : Channel Close) -> L ()
fork : ((1 chan : Server p) -> L ()) -> L {use=1} (Client p)

Utils : Protocol ()
Utils = do
  cmd <- Request Command
  case cmd of
       Add => do
         _ <- Request (Int, Int)
         _ <- Respond Int
         Done
       Reverse => do
         _ <- Request String
         _ <- Respond String
         Done

utilServer : (1 chan : Server Utils) -> L ()
utilServer chan = do
  cmd # chan <- recv chan
  case cmd of
       Add => do
         (x, y) # chan <- recv chan
         chan <- send chan (x + y)
         close chan
       Reverse => do
         str # chan <- recv chan
         chan <- send chan (reverse str)
         close chan

