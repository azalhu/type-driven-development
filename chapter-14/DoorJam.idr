module DoorJam

%default total

data DoorState = DoorClosed | DoorOpen

data DoorResult = OK | Jammed

DoorOpenResult : DoorResult -> DoorState
DoorOpenResult OK = DoorOpen
DoorOpenResult Jammed = DoorClosed

data DoorCmd : (ty : Type) -> DoorState -> (ty -> DoorState) -> Type where
  Open : DoorCmd DoorResult DoorClosed DoorOpenResult
  Close : DoorCmd () DoorOpen (const DoorClosed)
  RingBell : DoorCmd () DoorClosed (const DoorClosed)
  --------
  Display : String -> DoorCmd () state (const state)
  --------
  Pure : (res : ty) -> DoorCmd ty (state_fn res) state_fn
  (>>=) : DoorCmd a state1 state2_fn ->
          ((res : a) -> DoorCmd b (state2_fn res) state3_fn) ->
          DoorCmd b state1 state3_fn
  (>>) : DoorCmd () state1 state2_fn ->
         DoorCmd b (state2_fn ()) state3_fn ->
         DoorCmd b state1 state3_fn

doorProg : DoorCmd () DoorClosed (const DoorClosed)
doorProg = do
  RingBell
  OK <- Open
  | Jammed => Display "Door Jammed"
  Display "Glad To Be Of Service"
  Close
  OK <- Open
  | Jammed => Display "Door Jammed"
  Display "Glad To Be Of Service"
  Close

logOpen : DoorCmd DoorResult DoorClosed (\res => case res of
                                        OK => DoorOpen
                                        Jammed => DoorClosed)
logOpen = do
  Display "Trying to open the door"
  OK <- Open
  | Jammed => do
    Display "Jammed"
    Pure Jammed
  Display "Success"
  Pure OK

