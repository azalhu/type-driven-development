module Vending

%default total

Pounds : Type
Pounds = Nat

Chocs : Type
Chocs = Nat

VendState : Type
VendState = (Pounds, Chocs)

data CoinResult = Inserted | Rejected

data Input = COIN
           | VEND
           | CHANGE
           | REFILL Nat

MachineCoinResult : VendState -> CoinResult -> VendState
MachineCoinResult (pounds, chocs) Inserted = (S pounds, chocs)
MachineCoinResult (pounds, chocs) Rejected = (pounds, chocs)

data MachineCmd : (ty : Type) ->
                  VendState ->
                  (ty -> VendState) ->
                  Type where
  InsertCoin : MachineCmd CoinResult (pounds, chocs) (MachineCoinResult (pounds, chocs))
  Vend       : MachineCmd () (S pounds, S chocs) (const (pounds, chocs))
  GetCoins   : MachineCmd () (pounds, chocs) (const (Z, chocs))
  Refill     : (bars : Nat) -> MachineCmd () (Z, chocs) (const (Z, bars + chocs))
  -----------
  Display    : String -> MachineCmd () state (const state)
  GetInput   : MachineCmd (Maybe Input) state (const state)
  -----------
  Pure       : (res : ty) -> MachineCmd ty (state_fn res) state_fn
  (>>=)      : MachineCmd a state1 state2_fn ->
               ((res : a) -> MachineCmd b (state2_fn res) state3_fn) ->
               MachineCmd b state1 state3_fn
  (>>)       : MachineCmd () state1 state2_fn ->
               Lazy (MachineCmd b (state2_fn ()) state3_fn) ->
               MachineCmd b state1 state3_fn

data MachineIO : VendState -> Type where
  Do : MachineCmd a state1 state2_fn ->
       ((res : a) -> Inf (MachineIO (state2_fn res))) ->
       MachineIO state1
  Seq : MachineCmd () state1 state2_fn ->
        Inf (MachineIO (state2_fn ())) ->
        MachineIO state1

namespace MachineDo
  export
  (>>=) : MachineCmd a state1 state2_fn ->
          ((res : a) -> Inf (MachineIO (state2_fn res))) ->
          MachineIO state1
  (>>=) = Do

  export
  (>>) : MachineCmd () state1 state2_fn ->
         Inf (MachineIO (state2_fn ())) ->
         MachineIO state1
  (>>) = Seq

mutual
  vend : {pounds : _} -> {chocs : _ } -> MachineIO (pounds, chocs)
  vend {pounds = S p} {chocs = S c} = do
    Vend
    Display "Enjoy!"
    machineLoop
  vend {pounds = Z} = do
    Display "Insert a coin"
    machineLoop
  vend {chocs = Z} = do
    Display "Out of stock"
    machineLoop

  refill : {pounds : _} -> {chocs : _ } -> (num : Nat) -> MachineIO (pounds, chocs)
  refill {pounds = Z} num = do
    Refill num
    machineLoop
  refill _ = do
    Display "Can't refill: Coins in machine"
    machineLoop

  machineLoop : {pounds : _} -> {chocs : _ } -> MachineIO (pounds, chocs)
  machineLoop = do
    Just x <- GetInput
    | Nothing => do
      Display "Invalid input"
      machineLoop
    case x of
         COIN => do
           Inserted <- InsertCoin
           | Rejected => Display "Coin rejected" >> machineLoop
           machineLoop
         VEND => vend
         CHANGE => do
           GetCoins
           Display "Change returned"
           machineLoop
         REFILL num => refill num

