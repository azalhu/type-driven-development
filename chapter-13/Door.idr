module Door

%default total

data DoorState = DoorClosed | DoorOpen

data DoorCmd : Type ->
               DoorState ->
               DoorState ->
               Type where
  Open :     DoorCmd () DoorClosed DoorOpen
  Close :    DoorCmd () DoorOpen   DoorClosed
  RingBell : DoorCmd () state state
  ------
  Pure : ty -> DoorCmd ty st st
  (>>=) : DoorCmd a st1 st2 ->
          (a -> DoorCmd b st2 st3) ->
          DoorCmd b st1 st3
  (>>) : DoorCmd () st1 st2 ->
         Lazy (DoorCmd () st2 st3) ->
         DoorCmd () st1 st3

doorProg : DoorCmd () DoorClosed DoorClosed
doorProg = do
  RingBell
  Open
  RingBell
  Close

