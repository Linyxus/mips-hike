{-# LANGUAGE OverloadedStrings #-}
module Lang.CLike.Parser (parseFile, parseExpr) where
import Data.Text.IO as TIO
import Data.Text (Text, pack)
import Data.Void
import Control.Applicative (liftA2)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Lang.CLike.Types

type Parser = Parsec Void Text

parseFile :: FilePath -> IO (Either String Prog)
parseFile f = g . parse (pProg <* eof) f <$> TIO.readFile f
  where g (Left x) = Left $ errorBundlePretty x
        g (Right x) = pure x

parseExpr :: Text -> Either String Expr
parseExpr s = f $ parse pExpr "test" s
  where f (Left e) = Left $ errorBundlePretty e
        f (Right x) = pure x

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

intP :: Parser Int
intP = L.signed sc $ lexeme L.decimal

identP :: Parser Text
identP = lexeme . fmap pack $ liftA2 (:) letterChar (many alphaNumChar)

paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

bracket :: Parser a -> Parser a
bracket = between (symbol "{") (symbol "}")

pTerm :: Parser Expr
pTerm = choice [ paren pExpr
               , EIdent <$> identP
               , ENum <$> intP
               ]

pProg :: Parser Prog
pProg = Prog <$> pVarDefs <*> pMemDefs <*> pStmts

pExpr :: Parser Expr
pExpr = makeExprParser pTerm opTable

pVarDefs :: Parser [VarDef]
pVarDefs = symbol "var" >> identP `sepBy1` symbol "," <* symbol ";"

pMemDefs :: Parser [MemDef]
pMemDefs = symbol "mem" >> pMemDef `sepBy1` symbol "," <* symbol ";"

pStmt :: Parser Stmt
pStmt = choice $ try <$> [ pRefAssign
                         , pAssign
                         , pWhile
                         , pIf
                         , pPrint
                         ]

pStmts :: Parser Stmts
pStmts = many pStmt

pAssign :: Parser Stmt
pAssign = Assign <$> identP <* symbol "=" <*> pExpr

pRefAssign :: Parser Stmt
pRefAssign = RefAssign <$> pExpr <* symbol ".=" <*> pExpr

pWhile :: Parser Stmt
pWhile = symbol "while" >> While <$> pExpr <*> bracket pStmts

pPrint :: Parser Stmt
pPrint = symbol "print" >> Print <$> pExpr

pIf :: Parser Stmt
pIf = symbol "if" >> If <$> pExpr <*> bracket pStmts
      <*> optional (symbol "else" >> bracket pStmts)

pMemDef :: Parser MemDef
pMemDef = choice $ try <$> [ pMemValDef, pMemListDef ]

pMemValDef :: Parser MemDef
pMemValDef = MemVal <$> identP <* symbol "=" <*> intP

pMemListDef :: Parser MemDef
pMemListDef = MemList <$> identP <* symbol "[]" <* symbol "=" <*> pList

pList :: Parser [Int]
pList = between (symbol "[") (symbol "]") $ intP `sepBy1` symbol ","

opTable :: [[Operator Parser Expr]]
opTable = [ [ prefix "-" ENeg
            , prefix "!" ENot
            , prefix "*" ERef
            , prefix "&" EAddr
            ]
          , [ binary "*" EMult
            , binary "/" EDiv
            ]
          , [ binary "+" EPlus
            , binary "-" EMinus
            ]
          , [ binary "<" ELt
            , binary ">" EGt
            , binary "<=" ELe
            , binary ">=" EGe
            , binary "==" EEq
            , binary "!=" ENeq
            ]
          , [ binary "&&" EAnd
            , binary "||" EOr
            ]
          ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)
