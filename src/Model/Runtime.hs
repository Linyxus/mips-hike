{-# LANGUAGE RankNTypes #-}
module Model.Runtime where
import Control.Lens
import Control.Monad.State (lift, StateT, evalStateT)
import Control.Monad.Writer (tell)
import Control.Applicative (liftA2)
import qualified Data.Map as M
import Data.Bits hiding (xor)
import qualified Data.Bits as B
import GHC.Int (Int32)
import Model.Types
import qualified Model.Types as T

getReg :: Reg -> Machine Int32
getReg r = readReg r <$> use regfile

setReg :: Reg -> Int32 -> Machine ()
setReg R0 _ = pure ()
setReg r v = regfile . at r .= Just v

getMem :: Int32 -> Machine MemoryUnit
getMem x = readMem x <$> use mem

getMemV :: Int32 -> Machine Int32
getMemV = fmap f . getMem
  where f (MValue x) = x
        f (MInst _) = error "Memory unit is not value"

getMemI :: Int32 -> Machine Inst
getMemI = fmap f . getMem
  where f (MValue x) = error "Memory unit is not instruction"
        f (MInst i) = i

setMem :: Int32 -> MemoryUnit -> Machine ()
setMem k v = mem . at (k `Prelude.div` 4) .= Just v

mov :: Reg -> Reg -> Machine ()
mov R0 _ = pure ()
mov rd rs = getReg rs >>= setReg rd

lwi :: Reg -> Int32 -> Machine ()
lwi R0 _ = pure ()
lwi rd i = setReg rd i

lwda :: Reg -> Int32 -> Machine ()
lwda R0 _ = pure ()
lwda rd imm = f >>= getMemV >>= setReg rd
  where f = flip calcAddr imm <$> use pc

lwia :: Reg -> Int32 -> Machine ()
lwia R0 _ = pure ()
lwia rd imm = f >>= getMemV >>= getMemV >>= setReg rd
  where f = flip calcAddr imm <$> use pc

lwria :: Reg -> Reg -> Machine ()
lwria R0 _ = pure ()
lwria rd rs = getReg rs >>= getMemV >>= setReg rd

lw :: Reg -> Reg -> Int32 -> Machine ()
lw rd rs imm = (+imm) <$> getReg rs >>= getMemV >>= setReg rd

swda :: Reg -> Int32 -> Machine ()
swda rs imm = do
  addr <- flip calcAddr imm <$> use pc
  v <- getReg rs
  setMem addr $ MValue v

swia :: Reg -> Int32 -> Machine ()
swia rs imm = do
  addr <- flip calcAddr imm <$> use pc >>= getMemV
  v <- getReg rs
  setMem addr $ MValue v

swria :: Reg -> Reg -> Machine ()
swria rs rt = do
  addr <- getReg rt
  v <- getReg rs
  setMem addr $ MValue v

sw :: Reg -> Reg -> Int32 -> Machine ()
sw rs rt imm = do
  addr <- (+imm) <$> getReg rt
  v <- getReg rs
  setMem addr $ MValue v

push :: Reg -> Machine ()
push rs = do
  addr <- use sp
  v <- getReg rs
  setMem addr $ MValue v
  sp %= subtract 4

pop :: Reg -> Machine ()
pop rd = sp %= (+4) >> use sp >>= getMemV >>= setReg rd

pushi :: Int32 -> Machine ()
pushi imm = use sp >>= flip setMem (MValue imm) >> sp %= subtract 4

add :: Reg -> Reg -> Reg -> Machine ()
add rd rs rt = liftA2 (+) (getReg rs) (getReg rt) >>= setReg rd

addi :: Reg -> Reg -> Int32 -> Machine ()
addi rd rs imm = (+imm) <$> getReg rs >>= setReg rd

addu :: Reg -> Reg -> Reg -> Machine ()
addu = add

addiu :: Reg -> Reg -> Int32 -> Machine ()
addiu = addi

sub :: Reg -> Reg -> Reg -> Machine ()
sub rd rs rt = liftA2 (-) (getReg rs) (getReg rt) >>= setReg rd

subu :: Reg -> Reg -> Reg -> Machine ()
subu = sub

negu :: Reg -> Reg -> Machine ()
negu rd rs = f <$> getReg rs >>= setReg rd
  where f x = -x

div :: Reg -> Reg -> Machine ()
div rs rt = liftA2 Prelude.div (getReg rs) (getReg rt) >>= (lo .=)
            >> liftA2 mod (getReg rs) (getReg rt) >>= (hi .=)

divu :: Reg -> Reg -> Machine ()
divu = Model.Runtime.div

mult :: Reg -> Reg -> Machine ()
mult rs rt = liftA2 g fs ft >>= (lo .=)
             >> liftA2 f fs ft >>= (hi .=)
  where fs = getReg rs
        ft = getReg rt
        f x y = fromIntegral $ Prelude.div (i * j) 2^32 where i = fromIntegral x :: Integer
                                                              j = fromIntegral y :: Integer
        g x y = fromIntegral $ mod (i * j) (2^32) where i = fromIntegral x :: Integer
                                                        j = fromIntegral y :: Integer

f :: Int32 -> Int32 -> Int32
f x y = fromIntegral $ mod (i * j) (2^32) where i = fromIntegral x :: Integer
                                                j = fromIntegral y :: Integer


multu :: Reg -> Reg -> Machine ()
multu = mult

mfhi :: Reg -> Machine ()
mfhi rd = use hi >>= setReg rd

mflo :: Reg -> Machine ()
mflo rd = use lo >>= setReg rd

and :: Reg -> Reg -> Reg -> Machine ()
and rd rs rt = liftA2 (.&.) (getReg rs) (getReg rt) >>= setReg rd

andi :: Reg -> Reg -> Int32 -> Machine ()
andi rd rs imm = (.&. imm) <$> getReg rs >>= setReg rd

or :: Reg -> Reg -> Reg -> Machine ()
or rd rs rt = liftA2 (.|.) (getReg rs) (getReg rt) >>= setReg rd

ori :: Reg -> Reg -> Int32 -> Machine ()
ori rd rs imm = (.|. imm) <$> getReg rs >>= setReg rd

xor :: Reg -> Reg -> Reg -> Machine ()
xor rd rs rt = liftA2 B.xor (getReg rs) (getReg rt) >>= setReg rd

xori :: Reg -> Reg -> Int32 -> Machine ()
xori rd rs imm = B.xor imm <$> getReg rs >>= setReg rd

not :: Reg -> Reg -> Machine ()
not rd rs = B.xor (-1) <$> getReg rs >>= setReg rd

nop :: Machine ()
nop = pure ()

slt :: Reg -> Reg -> Reg -> Machine ()
slt rd rs rt = liftA2 f fs ft >>= setReg rd
  where fs = getReg rs
        ft = getReg rt
        f x y = if x < y then 1 else 0

slti :: Reg -> Reg -> Int32 -> Machine ()
slti rd rs imm = f imm <$> getReg rs >>= setReg rd
  where f y x = if x < y then 1 else 0

sltu :: Reg -> Reg -> Reg -> Machine ()
sltu = slt

sltiu :: Reg -> Reg -> Int32 -> Machine ()
sltiu = slti

b :: Int32 -> Machine ()
b imm = pc %= (+imm)

bal :: Int32 -> Machine ()
bal imm = use pc >>= setReg R31 >> pc %= (+imm)

mkBranch :: (Int32 -> Int32 -> Bool) -> Reg -> Reg -> Int32 -> Machine ()
mkBranch p rs rt imm = do
  vs <- getReg rs
  vt <- getReg rt
  if p vs vt then pc %= (+imm) else pure ()

beq :: Reg -> Reg -> Int32 -> Machine ()
beq = mkBranch (==)

bne :: Reg -> Reg -> Int32 -> Machine ()
bne = mkBranch (/=)

bgez :: Reg -> Int32 -> Machine ()
bgez = flip (mkBranch (>=)) R0

bgtz :: Reg -> Int32 -> Machine ()
bgtz = flip (mkBranch (>)) R0

blez :: Reg -> Int32 -> Machine ()
blez = flip (mkBranch (<=)) R0

j :: Int32 -> Machine ()
j imm = f >>= (pc .=)
  where f = flip calcAddr28 imm <$> use pc

jal :: Int32 -> Machine ()
jal imm = use pc >>= setReg R31 >> f >>= (pc .=)
  where f = flip calcAddr28 imm <$> use pc

jr :: Reg -> Machine ()
jr r = getReg r >>= (pc .=)

jral :: Reg -> Machine ()
jral r = use pc >>= setReg R31 >> getReg r >>= (pc .=)

break :: Machine ()
break = sr . srBreak .= True

syscall :: Machine ()
syscall = getReg R1 >>= f
  where f 0 = getReg R2 >>= disp
        f _ = error "unsupported syscall id"
        disp x = lift $ print x

eret :: Machine ()
eret = sr . srRunInte .= False >> (use (sr . srIpc) >>= assign pc)

mfc0 :: Reg -> Reg -> Int32 -> Machine ()
mfc0 = const . const . const $ pure ()

mtc0 :: Reg -> Reg -> Int32 -> Machine ()
mtc0 = const . const . const $ pure ()

disp :: String -> Machine ()
disp = lift . putStr

inte :: Int32 -> Machine ()
inte = (sr . srIVec .=)

ei :: Machine ()
ei = sr . srEI .= True

di :: Machine ()
di = sr . srEI .= False

interrupt :: Machine ()
interrupt = do
  sr . srEI .= False
  sr . srRunInte .= True
  epc <- use (sr . srIVec) >>= getEpc
  use pc >>= assign (sr . srIpc)
  pc .= epc
  sr . srIVec .= 0

getEpc :: Int32 -> Machine Int32
getEpc n = f . M.lookup n <$> use inteVec
  where f (Just x) = x
        f Nothing = error "Unknown exception vector"

calcAddr28 :: Int32 -> Int32 -> Int32
calcAddr28 pc imm = f pc + g imm
  where f x = (x `Prelude.div` 2^28) * 2^28
        g x = (x `mod` 2^26) * 4

calcAddr :: Int32 -> Int32 -> Int32
calcAddr pc imm = f pc + g imm
  where f x = (x `Prelude.div` 2^18) * 2^18
        g x = (x `mod` 2^16) * 4
