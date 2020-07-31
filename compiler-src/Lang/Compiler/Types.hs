{-# LANGUAGE TemplateHaskell #-}
module Lang.Compiler.Types where
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)
import Control.Lens
import Data.Text (Text)
import Data.Map as P
import Data.Char (toLower)

import qualified Lang.CLike.Types as C
import qualified Lang.MipsLike.Types as M
import Lang.MipsLike.Types (Reg)

type Comp = StateT CompState CompE

type CompE = ExceptT CompError IO

data CompState = CompState { _baseReg :: M.Reg
                           , _tagCnt :: Int
                           , _memTag :: P.Map Text Int
                           , _vars :: P.Map Text M.Reg
                           , _tagMap :: P.Map Int Int
                           , _currentPos :: Int
                           } deriving (Eq, Show)

data CompError = DuplicateName
               | TooManyVariables
               | OutOfRegister
               | NameUndefined Text
               | TagUndefined Int
               | ParseError String
               | OnlyGetAddressInMem
               deriving (Eq, Show)

data Inst = Mov Reg Reg
          | Lwi Reg Value
          | Lwda Reg Value
          | Lwia Reg Value
          | Lwria Reg Reg
          | Lw Reg Reg Value
          | Swda Reg Value
          | Swia Reg Value
          | Swria Reg Reg
          | Sw Reg Reg Value
          | Push Reg
          | Pop Reg
          | Pushi Value
          | Add Reg Reg Reg
          | Addi Reg Reg Value
          | Addu Reg Reg Reg
          | Addiu Reg Reg Value
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
          | Andi Reg Reg Value
          | Or Reg Reg Reg
          | Ori Reg Reg Value
          | Xor Reg Reg Reg
          | Xori Reg Reg Value
          | Not Reg Reg
          | Nop
          | Slt Reg Reg Reg
          | Slti Reg Reg Value
          | Sltu Reg Reg Reg
          | Sltiu Reg Reg Value
          | B Value
          | Bal Value
          | Beq Reg Reg Value
          | Bne Reg Reg Value
          | Bgez Reg Value
          | Bgtz Reg Value
          | Blez Reg Value
          | J Value
          | Jal Value
          | Jr Reg
          | Jral Reg
          | Break
          | Syscall
          | Eret
          | Mfc0 Reg Reg Value
          | Mtc0 Reg Reg Value
          | Dw Value
          deriving (Eq, Show)

showInst :: Inst -> String
showInst = fmap toLower . show

data Value = VInt Int
           | VAbs Int
           | VRel Int
           | VAddr Int
           deriving (Eq)

showValue :: Value -> String
showValue (VInt x) = show x
showValue (VAbs t) = "." ++ show t
showValue (VRel t) = "." ++ show t

instance Show Value where show = showValue

data Asm = ATagged Int Inst
         | AInst Inst
         deriving (Eq)

instance Show Asm where show = showAsm

showAsm :: Asm -> String
showAsm (ATagged t i) = show t ++ ":" ++ showInst i
showAsm (AInst i) = showInst i

makeLenses ''CompState
