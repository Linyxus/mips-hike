module Lang.MipsLike.Types where
import Data.Map as M
import Data.Char (toLower)

-- | 存储指令的数据结构
data Inst = Mov Reg Reg
          | Lwi Reg Int
          | Lwda Reg Int
          | Lwia Reg Int
          | Lwria Reg Reg
          | Lw Reg Reg Int
          | Swda Reg Int
          | Swia Reg Int
          | Swria Reg Reg
          | Sw Reg Reg Int
          | Push Reg
          | Pop Reg
          | Pushi Int
          | Add Reg Reg Reg
          | Addi Reg Reg Int
          | Addu Reg Reg Reg
          | Addiu Reg Reg Int
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
          | Andi Reg Reg Int
          | Or Reg Reg Reg
          | Ori Reg Reg Int
          | Xor Reg Reg Reg
          | Xori Reg Reg Int
          | Not Reg Reg
          | Nop
          | Slt Reg Reg Reg
          | Slti Reg Reg Int
          | Sltu Reg Reg Reg
          | Sltiu Reg Reg Int
          | B Int
          | Bal Int
          | Beq Reg Reg Int
          | Bne Reg Reg Int
          | Bgez Reg Int
          | Bgtz Reg Int
          | Blez Reg Int
          | J Int
          | Jal Int
          | Jr Reg
          | Jral Reg
          | Break
          | Syscall
          | Eret
          | Mfc0 Reg Reg Int
          | Mtc0 Reg Reg Int
          | Disp String
          | Dw Int
          deriving (Eq, Show)

showInst :: Inst -> String
showInst = Prelude.filter f . fmap toLower . show
  where f '(' = False
        f ')' = False
        f _ = True

-- | 寄存器名称
data Reg = R0 | R1 | R2 | R3
         | R4 | R5 | R6 | R7
         | R8 | R9 | R10 | R11
         | R12 | R13 | R14 | R15
         | R16 | R17 | R18 | R19
         | R20 | R21 | R22 | R23
         | R24 | R25 | R26 | R27
         | R28 | R29 | R30 | R31
         deriving (Eq, Ord, Enum)

instance Show Reg where show = showReg

showReg :: Reg -> String
showReg r = f $ M.lookup r m
  where m = fromList $ zip [R0 .. R31] [0 ..]
        f (Just x) = '$' : show x
