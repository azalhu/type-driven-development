module ATM

import Data.String
import Data.Vect

%default total

PIN : Type
PIN = Vect 4 Char

data ATMState = Ready | CardInserted | Session

data HasCard : ATMState -> Type where
  HasCI : HasCard CardInserted
  HasSession : HasCard Session

data PINCheck = CorrectPIN | IncorrectPIN

data ATMCmd : (ty : Type) -> ATMState -> (ty -> ATMState) -> Type where
  InsertCard : ATMCmd () Ready (const CardInserted)
  EjectCard : {auto prf : HasCard state} -> ATMCmd () state (const Ready)
  GetPIN : ATMCmd PIN CardInserted (const CardInserted)
  CheckPIN : PIN -> ATMCmd PINCheck CardInserted (\res => case res of
                                                 CorrectPIN => Session
                                                 IncorrectPIN => CardInserted)
  Dispense : Nat -> ATMCmd () Session (const Session)
  ----------
  GetAmount : ATMCmd Nat state (const state)
  Message : String -> ATMCmd () state (const state)
  ----------
  Pure : (res : ty) -> ATMCmd ty (state_fn res) state_fn
  (>>=) : ATMCmd a state1 state2_fn ->
          ((res : a) -> ATMCmd b (state2_fn res) state3_fn) ->
          ATMCmd b state1 state3_fn
  (>>) : ATMCmd () state1 state2_fn ->
         ATMCmd b (state2_fn ()) state3_fn ->
         ATMCmd b state1 state3_fn

atm : ATMCmd () Ready (const Ready)
atm = do
  InsertCard
  pin <- GetPIN
  pinOK <- CheckPIN pin
  case pinOK of
       CorrectPIN => do
         cash <- GetAmount
         Dispense cash
         EjectCard
       IncorrectPIN => EjectCard

atm' : ATMCmd () Ready (const Ready)
atm' = do
  InsertCard
  pin <- GetPIN
  cash <- GetAmount
  pinOK <- CheckPIN pin
  Message "Checking Card"
  case pinOK of
       CorrectPIN => do
         Dispense cash
         EjectCard
         Message "Please remove your card and cash"
       IncorrectPIN => do
         Message "Incorrect PIN"
         EjectCard

testPIN : Vect 4 Char
testPIN = ['1', '2', '3', '4']

readPIN : IO PIN
readPIN = readVect 4
  where
    readVect : (n : Nat) -> IO (Vect n Char)
    readVect Z = pure []
    readVect (S k) = do
      chr <- getChar
      chrs <- readVect k
      pure (chr :: chrs)

runATM : ATMCmd res inState outState_fn -> IO res
runATM InsertCard = do
  putStrLn "Please insert your card (press enter)"
  _ <- getLine
  pure ()
runATM EjectCard = putStrLn "Card ejected"
runATM GetPIN = do
  putStr "Enter PIN: "
  readPIN
runATM (CheckPIN pin) =
  if pin == testPIN
     then pure CorrectPIN
     else pure IncorrectPIN
runATM (Dispense amount) = putStrLn ("Here is " ++ show amount)
runATM GetAmount = do
  putStr "How much would you like? "
  amount <- getLine
  pure (stringToNatOrZ amount)
runATM (Message msg) = putStrLn msg
runATM (Pure res) = pure res
runATM (cmd >>= next) = do
  res <- runATM cmd
  runATM (next res)
runATM (cmd >> next) = do
  runATM cmd
  runATM next

insertEject : ATMCmd () Ready (const Ready)
insertEject = do
  InsertCard
  EjectCard
  --EjectCard

