{-# LANGUAGE OverloadedStrings #-}
module Model.Memory.Parsers where
import Control.Applicative (liftA2)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Text.Megaparsec hiding (option, token)
import Text.Megaparsec.Char hiding (token)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Data.Map as M
import GHC.Int (Int32)

import Model.Types

type Parser = Parsec Void Text

pMemUnit :: Parser MemoryUnit
pMemUnit = choice [ MInst <$> pInst
                  , MValue <$> pValue
                  ]

pMemUnits :: Parser [MemoryUnit]
pMemUnits = many pMemUnit

loadMem :: String -> IO Memory
loadMem p = parse (pMemUnits <* eof) p <$> TIO.readFile p >>= f
  where f (Left e) = error $ errorBundlePretty e
        f (Right x) = pure $ buildMem x

loadInteService :: String -> Int32 -> Memory -> IO Memory
loadInteService p n m = parse (pMemUnits <* eof) p <$> TIO.readFile p >>= f
  where f (Left e) = error $ errorBundlePretty e
        f (Right x) = pure $ insertMemAt m n x

insertMemAt :: Memory -> Int32 -> [MemoryUnit] -> Memory
insertMemAt m n = go [n..]
  where go _ [] = m
        go (x:xs) (u:us) = M.insert x u $ go xs us

buildMem :: [MemoryUnit] -> Memory
buildMem = go [0..]
  where go _ [] = emptyMem
        go (x:xs) (u:us) = M.insert x u $ go xs us

intP :: Parser Int32
intP = read <$> liftA2 (++) (pure <$> char '-' <|> return []) (some digitChar)

symbol :: Text -> Parser Text
symbol s = string s <* space

sc :: Parser a -> Parser a
sc = (<* space)

pInst :: Parser Inst
pInst = choice $ try <$> [ symbol "mov" >> Mov <$> pReg <*> pReg
                         , symbol "lwi" >> Lwi <$> pReg <*> pImm
                         , symbol "lwda" >> Lwda <$> pReg <*> pImm
                         , symbol "lwia" >> Lwia <$> pReg <*> pImm
                         , symbol "lwria" >> Lwria <$> pReg <*> pReg
                         , symbol "lw" >> Lw <$> pReg <*> pReg <*> pImm
                         , symbol "swda" >> Swda <$> pReg <*> pImm
                         , symbol "swia" >> Swia <$> pReg <*> pImm
                         , symbol "swria" >> Swria <$> pReg <*> pReg
                         , symbol "sw" >> Sw <$> pReg <*> pReg <*> pImm
                         , symbol "push" >> Push <$> pReg
                         , symbol "pop" >> Pop <$> pReg
                         , symbol "pushi" >> Pushi <$> pImm
                         , symbol "add" >> Add <$> pReg <*> pReg <*> pReg
                         , symbol "addi" >> Addi <$> pReg <*> pReg <*> pImm
                         , symbol "addu" >> Addu <$> pReg <*> pReg <*> pReg
                         , symbol "addiu" >> Addiu <$> pReg <*> pReg <*> pImm
                         , symbol "sub" >> Sub <$> pReg <*> pReg <*> pReg
                         , symbol "subu" >> Subu <$> pReg <*> pReg <*> pReg
                         , symbol "negu" >> Negu <$> pReg <*> pReg
                         , symbol "div" >> Div <$> pReg <*> pReg
                         , symbol "divu" >> Divu <$> pReg <*> pReg
                         , symbol "mult" >> Mult <$> pReg <*> pReg
                         , symbol "multu" >> Multu <$> pReg <*> pReg
                         , symbol "mfhi" >> Mfhi <$> pReg
                         , symbol "mflo" >> Mflo <$> pReg
                         , symbol "and" >> And <$> pReg <*> pReg <*> pReg
                         , symbol "andi" >> Andi <$> pReg <*> pReg <*> pImm
                         , symbol "or" >> Or <$> pReg <*> pReg <*> pReg
                         , symbol "ori" >> Ori <$> pReg <*> pReg <*> pImm
                         , symbol "xor" >> Xor <$> pReg <*> pReg <*> pReg
                         , symbol "xori" >> Xori <$> pReg <*> pReg <*> pImm
                         , symbol "not" >> Not <$> pReg <*> pReg
                         , Nop <$ symbol "nop"
                         , symbol "slt" >> Slt <$> pReg <*> pReg <*> pReg
                         , symbol "slti" >> Slti <$> pReg <*> pReg <*> pImm
                         , symbol "sltu" >> Sltu <$> pReg <*> pReg <*> pReg
                         , symbol "sltiu" >> Sltiu <$> pReg <*> pReg <*> pImm
                         , symbol "b" >> B <$> pImm
                         , symbol "bal" >> Bal <$> pImm
                         , symbol "beq" >> Beq <$> pReg <*> pReg <*> pImm
                         , symbol "bne" >> Bne <$> pReg <*> pReg <*> pImm
                         , symbol "bgez" >> Bgez <$> pReg <*> pImm
                         , symbol "bgtz" >> Bgtz <$> pReg <*> pImm
                         , symbol "blez" >> Blez <$> pReg <*> pImm
                         , symbol "j" >> J <$> pImm
                         , symbol "jal" >> Jal <$> pImm
                         , symbol "jr" >> Jr <$> pReg
                         , symbol "jral" >> Jral <$> pReg
                         , Break <$ symbol "break"
                         , Syscall <$ symbol "syscall"
                         , Eret <$ symbol "eret"
                         , symbol "mfc0" >> Mfc0 <$> pReg <*> pReg <*> pImm
                         , symbol "mtc0" >> Mtc0 <$> pReg <*> pReg <*> pImm
                         , symbol "disp" >> Disp <$> pString
                         , symbol "inte" >> Inte <$> pImm
                         , EI <$ symbol "ei"
                         , DI <$ symbol "di"
                         ]

pValue :: Parser Int32
pValue = symbol "dw" >> intP <* space

pReg = sc . choice $ try . mkP <$> reverse [0 .. 31]
  where mkP x = ([R0 .. R31] !! x) <$ (char '$' >> mkIntP x)
        mkIntP :: Int -> Parser Text
        mkIntP x = string $ pack $ show x

pImm :: Parser Int32
pImm = sc intP

pString :: Parser String
pString = sc $ char '\"' *> manyTill L.charLiteral (char '\"')
