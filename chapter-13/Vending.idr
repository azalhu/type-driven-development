module Vending

%default total

Pounds : Type
Pounds = Nat

Chocs : Type
Chocs = Nat

VendState : Type
VendState = (Pounds, Chocs)

data Input = COIN
           | VEND
           | CHANGE
           | REFILL Nat

data MachineCmd : Type ->
                  VendState ->
                  VendState ->
                  Type where
  InsertCoin : MachineCmd () (pounds, chocs) (S pounds, chocs)
  Vend       : MachineCmd () (S pounds, S chocs) (pounds, chocs)
  GetCoins   : MachineCmd () (pounds, chocs) (Z, chocs)
  Refill     : (bars : Nat) -> MachineCmd () (Z, chocs) (Z, bars + chocs)
  -----------
  Display    : String -> MachineCmd () state state
  GetInput   : MachineCmd (Maybe Input) state state
  -----------
  Pure       : ty -> MachineCmd ty state state
  (>>=)      : MachineCmd a state1 state2 ->
               (a -> MachineCmd b state2 state3) ->
               MachineCmd b state1 state3
  (>>)       : MachineCmd () state1 state2 ->
               Lazy (MachineCmd b state2 state3) ->
               MachineCmd b state1 state3

data MachineIO : VendState -> Type where
  Do : MachineCmd a state1 state2 ->
       (a -> Inf (MachineIO state2)) ->
       MachineIO state1
  Seq : MachineCmd () state1 state2 ->
        Inf (MachineIO state2) ->
        MachineIO state1

namespace MachineDo
  export
  (>>=) : MachineCmd a state1 state2 ->
          (a -> Inf (MachineIO state2)) ->
          MachineIO state1
  (>>=) = Do

  export
  (>>) : MachineCmd () state1 state2 ->
         Inf (MachineIO state2) ->
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
           InsertCoin
           machineLoop
         VEND => vend
         CHANGE => do
           GetCoins
           Display "Change returned"
           machineLoop
         REFILL num => refill num

