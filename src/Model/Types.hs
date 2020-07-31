{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Model.Types where
import Control.Monad.State (StateT, evalStateT, execStateT)
import Control.Lens
import Data.Array hiding ((++))
import qualified Data.Map as M
import GHC.Int (Int32)

runMachine :: Machine a -> MachineState -> IO a
runMachine = evalStateT

traceMachine :: Machine a -> MachineState -> IO MachineState
traceMachine = execStateT

buildState :: Memory -> MachineState
buildState m = MachineState{..}
  where _sr = StateReg False 0 0 False False 0
        _hi = 0
        _lo = 0
        _pc = 0
        _sp = 1000000
        _mem = m
        _regfile = M.empty
        _inteVec = M.fromList [ (10, 10000) ]

emptyMem :: Memory
-- ^ 空内存，每个存储单元都填充为空指令
emptyMem = M.empty

type Machine = StateT MachineState IO

-- | 存储模型机状态的数据结构
data MachineState = MachineState { _sr :: StateReg
                                 , _hi :: Int32
                                 , _lo :: Int32
                                 , _pc :: Int32
                                 , _sp :: Int32
                                 , _mem :: Memory
                                 , _inteVec :: InteVec
                                 , _regfile :: RegFile
                                 } deriving (Eq, Show)

-- | 在模型机执行时的状态记录，描述当前操作的效果
type LogSlice = String

-- | 存储指令的数据结构
data Inst = Mov Reg Reg
          | Lwi Reg Int32
          | Lwda Reg Int32
          | Lwia Reg Int32
          | Lwria Reg Reg
          | Lw Reg Reg Int32
          | Swda Reg Int32
          | Swia Reg Int32
          | Swria Reg Reg
          | Sw Reg Reg Int32
          | Push Reg
          | Pop Reg
          | Pushi Int32
          | Add Reg Reg Reg
          | Addi Reg Reg Int32
          | Addu Reg Reg Reg
          | Addiu Reg Reg Int32
          | Sub Reg Reg Reg
          | Subu Reg Reg Reg
          | Negu Reg Reg
          | Div Reg Reg
          | Divu Reg Reg
          | Mult Reg Reg
          | Multu Reg Reg
          | Mfhi Reg
          | Mflo Reg
          | And Reg Reg Reg
          | Andi Reg Reg Int32
          | Or Reg Reg Reg
          | Ori Reg Reg Int32
          | Xor Reg Reg Reg
          | Xori Reg Reg Int32
          | Not Reg Reg
          | Nop
          | Slt Reg Reg Reg
          | Slti Reg Reg Int32
          | Sltu Reg Reg Reg
          | Sltiu Reg Reg Int32
          | B Int32
          | Bal Int32
          | Beq Reg Reg Int32
          | Bne Reg Reg Int32
          | Bgez Reg Int32
          | Bgtz Reg Int32
          | Blez Reg Int32
          | J Int32
          | Jal Int32
          | Jr Reg
          | Jral Reg
          | Break
          | Syscall
          | Eret
          | Mfc0 Reg Reg Int32
          | Mtc0 Reg Reg Int32
          | Disp String
          | Inte Int32
          | EI
          | DI
          deriving (Eq, Show)

-- | 内存单元，值可能是指令也可能是数据
data MemoryUnit = MValue Int32
                | MInst Inst
                deriving (Eq)

showUnit :: MemoryUnit -> String
showUnit (MValue x) = "(值: " ++ show x ++ ")"
showUnit (MInst i) = "(指令: " ++ show i ++ ")"

instance Show MemoryUnit where show = showUnit

-- | 内存，连续的内存单元
type Memory = M.Map Int32 MemoryUnit

readMem :: Int32 -> Memory -> MemoryUnit
-- ^ 读取内存的值，注意内存地址的低4位会被直接丢弃
readMem x m = f $ M.lookup (x `div` 4) m
  where f (Just x) = x
        f Nothing = MValue 0

-- | 寄存器名称
data Reg = R0 | R1 | R2 | R3
         | R4 | R5 | R6 | R7
         | R8 | R9 | R10 | R11
         | R12 | R13 | R14 | R15
         | R16 | R17 | R18 | R19
         | R20 | R21 | R22 | R23
         | R24 | R25 | R26 | R27
         | R28 | R29 | R30 | R31
         deriving (Eq, Show, Ord, Enum)

-- | 状态寄存器
data StateReg = StateReg { _srBreak :: Bool
                         , _srCp0 :: Int32
                         , _srIpc :: Int32
                         , _srEI :: Bool
                         , _srRunInte :: Bool
                         , _srIVec :: Int32
                         } deriving (Eq, Show)

-- ｜ 寄存器文件
type RegFile = M.Map Reg Int32

-- | 中断向量
type InteVec = M.Map Int32 Int32

-- | 读取寄存器的值
readReg :: Reg -> RegFile -> Int32
readReg R0 _ = 0
readReg r m = f $ M.lookup r m
  where f (Just x) = x
        f Nothing = 0

makeLenses ''MachineState
makeLenses ''StateReg
