module Model.Simple
  ( runFile
  , traceFile
  , run1
  , run
  , Machine
  , StateReg(..)
  , MachineState(..)
  , Inst(..)
  , MemoryUnit(..)
  , Memory(..)
  , runMachine
  , traceMachine
  , loadMem
  , buildState
  ) where
import Control.Lens

import Model.Types
import Model.Runtime as R
import Model.Memory.Parsers

runFile :: String -> String -> IO ()
runFile f ef = buildState <$> (loadMem f >>= loadInteService ef 2500) >>= runMachine run

traceFile :: String -> String -> IO MachineState
traceFile f ef = buildState <$> (loadMem f >>= loadInteService ef 2500) >>= traceMachine run

run1 :: Machine ()
run1 = do
  inst <- use pc >>= getMemI
  pc %= (+4)
  execInst inst

run :: Machine ()
run = run1 >> checkInte >> check

check :: Machine ()
check = use (sr . srBreak) >>= f
  where f True = pure ()
        f False = run

checkInte :: Machine ()
checkInte = do
  vec <- use $ sr . srIVec
  enInte <- use $ sr . srEI
  let int = enInte && vec /= 0
  if int then interrupt else pure ()

execInst :: Inst -> Machine ()
execInst (Mov a1 a2) = mov a1 a2
execInst (Lwi a1 a2) = lwi a1 a2
execInst (Lwda a1 a2) = lwda a1 a2
execInst (Lwia a1 a2) = lwia a1 a2
execInst (Lwria a1 a2) = lwria a1 a2
execInst (Lw a1 a2 a3) = lw a1 a2 a3
execInst (Swda a1 a2) = swda a1 a2
execInst (Swia a1 a2) = swia a1 a2
execInst (Swria a1 a2) = swria a1 a2
execInst (Sw a1 a2 a3) = sw a1 a2 a3
execInst (Push a1) = push a1
execInst (Pop a1) = pop a1
execInst (Pushi a1) = pushi a1
execInst (Add a1 a2 a3) = add a1 a2 a3
execInst (Addi a1 a2 a3) = addi a1 a2 a3
execInst (Addu a1 a2 a3) = addu a1 a2 a3
execInst (Addiu a1 a2 a3) = addiu a1 a2 a3
execInst (Sub a1 a2 a3) = sub a1 a2 a3
execInst (Subu a1 a2 a3) = subu a1 a2 a3
execInst (Negu a1 a2) = negu a1 a2
execInst (Div a1 a2) = R.div a1 a2
execInst (Divu a1 a2) = divu a1 a2
execInst (Mult a1 a2) = mult a1 a2
execInst (Multu a1 a2) = multu a1 a2
execInst (Mfhi a1) = mfhi a1
execInst (Mflo a1) = mflo a1
execInst (And a1 a2 a3) = R.and a1 a2 a3
execInst (Andi a1 a2 a3) = andi a1 a2 a3
execInst (Or a1 a2 a3) = R.or a1 a2 a3
execInst (Ori a1 a2 a3) = ori a1 a2 a3
execInst (Xor a1 a2 a3) = xor a1 a2 a3
execInst (Xori a1 a2 a3) = xori a1 a2 a3
execInst (Not a1 a2) = R.not a1 a2
execInst Nop = nop
execInst (Slt a1 a2 a3) = slt a1 a2 a3
execInst (Slti a1 a2 a3) = slti a1 a2 a3
execInst (Sltu a1 a2 a3) = sltu a1 a2 a3
execInst (Sltiu a1 a2 a3) = sltiu a1 a2 a3
execInst (B a1) = b a1
execInst (Bal a1) = bal a1
execInst (Beq a1 a2 a3) = beq a1 a2 a3
execInst (Bne a1 a2 a3) = bne a1 a2 a3
execInst (Bgez a1 a2) = bgez a1 a2
execInst (Bgtz a1 a2) = bgtz a1 a2
execInst (Blez a1 a2) = blez a1 a2
execInst (J a1) = j a1
execInst (Jal a1) = jal a1
execInst (Jr a1) = jr a1
execInst (Jral a1) = jral a1
execInst Break = R.break
execInst Syscall = syscall
execInst Eret = eret
execInst (Mfc0 a1 a2 a3) = mfc0 a1 a2 a3
execInst (Mtc0 a1 a2 a3) = mtc0 a1 a2 a3
execInst (Disp s) = disp s
execInst DI = di
execInst EI = ei
execInst (Inte n) = inte n
