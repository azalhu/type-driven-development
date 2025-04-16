module DoorMonad

%default total

data DoorState = DoorClosed | DoorOpen

--data DoorTrans : DoorState -> DoorState -> Type where
--  MkOpen : DoorTrans DoorClosed DoorOpen
--  MkClosed : DoorTrans DoorOpen DoorClosed
--  MkNoOp : DoorTrans st st
--
--DoorCmdType : DoorTrans st1 st2 -> Type

--DoorCmdType : DoorTransition -> (DoorState -> DoorState) -> Type
--DoorCmdType MkOpen = (DoorClosed -> DoorOpen)
--DoorCmdType MkClose = DoorOpen -> DoorClosed
--DoorCmdType MkNoOp st = st

--data DoorTrans : DoorState -> DoorState -> Type where
--  OpenDoor : DoorTrans DoorClosed DoorOpen
--  CloseDoor : DoorTrans DoorOpen DoorClosed
--  ClosedDoor : DoorTrans DoorClosed DoorClosed
--  ChangeDoor : DoorTrans st1 st2
--  StillDoor : DoorTrans st st
--
--data DoorCmd : DoorTrans _ _ ->
--               Type ->
--               Type where
--  Open     : DoorCmd OpenDoor ()
--  Close    : DoorCmd CloseDoor ()
--  RingBell : DoorCmd ClosedDoor ()
--  ------
--  Pure : ty -> DoorCmd StillDoor ty
--  Bind : DoorCmd ChangeDoor a ->
--         (a -> DoorCmd ChangeDoor b) ->
--         DoorCmd ChangeDoor b

data DoorCmd : DoorState ->
               DoorState ->
               Type ->
               Type where
  Open     : DoorCmd DoorClosed DoorOpen   ()
  Close    : DoorCmd DoorOpen   DoorClosed ()
  RingBell : DoorCmd DoorClosed DoorClosed ()
  ------
  Pure : ty -> DoorCmd st1 st2 ty
  Bind : DoorCmd st1 st2 a ->
         (a -> DoorCmd st2 st3 b) ->
         DoorCmd st1 st3 b

Functor (DoorCmd st1 st2) where
  map f dc = Bind dc (Pure . f)

--Applicative (DoorCmd st1 st2) where
--  pure = Pure
--  (<*>) f dc = Bind f ?rhs
--
--Monad (DoorCmd st1 st3) where
--  (>>=) = Bind

