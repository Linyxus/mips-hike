{-# LANGUAGE RecordWildCards #-}
module Lang.Compiler.Compile (parseFile, compileFileAsm, compileFile) where
import Control.Monad.Except (throwError, runExceptT)
import Control.Monad.State (evalStateT, runStateT, liftIO)
import Control.Lens
import Control.Applicative (liftA2)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Map as P

import qualified Lang.CLike.Types as C
import qualified Lang.MipsLike.Types as M
import Lang.CLike.Parser (parseFile, parseExpr)
import Lang.Compiler.Types

compileAsm :: C.Prog -> IO (Either CompError [Asm])
compileAsm p = runCompiler $ compileAsmM p

compileAsmM :: C.Prog -> Comp [Asm]
compileAsmM p@C.Prog{..} = checkName p >> buildVars varDef >>
                           flip (++)
                           <$> buildMem memDef
                           <*> fmap (++[AInst Break]) (compileStmts body)

compile :: C.Prog -> IO (Either CompError String)
compile p = runCompiler $ compileM p

compileM :: C.Prog -> Comp String
compileM p = fmap f $ do
  asm <- compileAsmM p
  buildTagMap asm
  fillTags asm
  where f = unlines . fmap M.showInst

compileFile :: FilePath -> IO (Either CompError String)
compileFile f = runCompiler $ liftIO (parseFile f) >>= g
  where g :: Either String C.Prog -> Comp String
        g (Left e) = throwError $ ParseError e
        g (Right x) = compileM x

compileFileAsm :: FilePath -> IO (Either CompError String)
compileFileAsm f = runCompiler $ liftIO (parseFile f) >>= g
  where g :: Either String C.Prog -> Comp String
        g (Left e) = throwError $ ParseError e
        g (Right x) = mconcat . fmap show <$> compileAsmM x

buildTagMap :: [Asm] -> Comp ()
buildTagMap code = currentPos .= 0 >> go code
  where go [] = return ()
        go ((ATagged t _):xs) = do
          pos <- use currentPos
          incr
          tagMap . at t .= Just pos
          go xs
        go (_:xs) = incr >> go xs
        incr = currentPos %= (+4)

fillTags :: [Asm] -> Comp [M.Inst]
fillTags code = currentPos .= 0 >> go (f <$> code)
  where go [] = return []
        go (x:xs) = (:) <$> g x <*> (incr >> go xs)
        f (AInst i) = i
        f (ATagged _ i) = i
        g :: Inst -> Comp M.Inst
        g (Mov r0 r1) = return $ M.Mov r0 r1
        g (Lwi r0 v1) = do
          i1 <- resolveValue v1
          return $ M.Lwi r0 i1
        g (Lwda r0 v1) = do
          i1 <- resolveValue v1
          return $ M.Lwda r0 i1
        g (Lwia r0 v1) = do
          i1 <- resolveValue v1
          return $ M.Lwia r0 i1
        g (Lwria r0 r1) = return $ M.Lwria r0 r1
        g (Lw r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Lw r0 r1 i2
        g (Swda r0 v1) = do
          i1 <- resolveValue v1
          return $ M.Swda r0 i1
        g (Swia r0 v1) = do
          i1 <- resolveValue v1
          return $ M.Swia r0 i1
        g (Swria r0 r1) = return $ M.Swria r0 r1
        g (Sw r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Sw r0 r1 i2
        g (Push r0) = return $ M.Push r0
        g (Pop r0) = return $ M.Pop r0
        g (Pushi v0) = do
          i0 <- resolveValue v0
          return $ M.Pushi i0
        g (Add r0 r1 r2) = return $ M.Add r0 r1 r2
        g (Addi r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Addi r0 r1 i2
        g (Addu r0 r1 r2) = return $ M.Addu r0 r1 r2
        g (Addiu r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Addiu r0 r1 i2
        g (Sub r0 r1 r2) = return $ M.Sub r0 r1 r2
        g (Subu r0 r1 r2) = return $ M.Subu r0 r1 r2
        g (Negu r0 r1) = return $ M.Negu r0 r1
        g (Div r0 r1) = return $ M.Div r0 r1
        g (Divu r0 r1) = return $ M.Divu r0 r1
        g (Mult r0 r1) = return $ M.Mult r0 r1
        g (Multu r0 r1) = return $ M.Multu r0 r1
        g (Mfhi r0) = return $ M.Mfhi r0
        g (Mflo r0) = return $ M.Mflo r0
        g (And r0 r1 r2) = return $ M.And r0 r1 r2
        g (Andi r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Andi r0 r1 i2
        g (Or r0 r1 r2) = return $ M.Or r0 r1 r2
        g (Ori r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Ori r0 r1 i2
        g (Xor r0 r1 r2) = return $ M.Xor r0 r1 r2
        g (Xori r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Xori r0 r1 i2
        g (Not r0 r1) = return $ M.Not r0 r1
        g Nop = return M.Nop
        g (Slt r0 r1 r2) = return $ M.Slt r0 r1 r2
        g (Slti r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Slti r0 r1 i2
        g (Sltu r0 r1 r2) = return $ M.Sltu r0 r1 r2
        g (Sltiu r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Sltiu r0 r1 i2
        g (B v0) = do
          i0 <- resolveValue v0
          return $ M.B i0
        g (Bal v0) = do
          i0 <- resolveValue v0
          return $ M.Bal i0
        g (Beq r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Beq r0 r1 i2
        g (Bne r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Bne r0 r1 i2
        g (Bgez r0 v1) = do
          i1 <- resolveValue v1
          return $ M.Bgez r0 i1
        g (Bgtz r0 v1) = do
          i1 <- resolveValue v1
          return $ M.Bgtz r0 i1
        g (Blez r0 v1) = do
          i1 <- resolveValue v1
          return $ M.Blez r0 i1
        g (J v0) = do
          i0 <- resolveValue v0
          return $ M.J i0
        g (Jal v0) = do
          i0 <- resolveValue v0
          return $ M.Jal i0
        g (Jr r0) = return $ M.Jr r0
        g (Jral r0) = return $ M.Jral r0
        g Break = return M.Break
        g Syscall = return M.Syscall
        g Eret = return M.Eret
        g (Mfc0 r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Mfc0 r0 r1 i2
        g (Mtc0 r0 r1 v2) = do
          i2 <- resolveValue v2
          return $ M.Mtc0 r0 r1 i2
        g (Dw v0) = do
          i0 <- resolveValue v0
          return $ M.Dw i0
        incr = currentPos %= (+4)

resolveValue :: Value -> Comp Int
resolveValue (VInt x) = pure x
resolveValue (VAbs t) = resolveAbs t
  where resolveAbs :: Int -> Comp Int
        resolveAbs t = do
          i <- P.lookup t <$> use tagMap
          case i of
            Nothing -> throwError $ TagUndefined t
            Just x -> return $ x `div` 4
resolveValue (VRel t) = resolveRel t
  where resolveRel :: Int -> Comp Int
        resolveRel t = do
          i <- P.lookup t <$> use tagMap
          case i of
            Nothing -> throwError $ TagUndefined t
            Just x -> do
              pos <- use currentPos
              return $ x - pos - 4
resolveValue (VAddr t) = do
  i <- P.lookup t <$> use tagMap
  case i of
    Nothing -> throwError $ TagUndefined t
    Just x -> return x

initState :: CompState
initState = CompState {..}
  where _baseReg = M.R20
        _tagCnt = 0
        _memTag = P.empty
        _vars = P.empty
        _tagMap = P.empty
        _currentPos = 0

runCompiler :: Comp a -> IO (Either CompError a)
runCompiler = runExceptT . flip evalStateT initState

execCompiler :: Comp a -> IO (Either CompError (a, CompState))
execCompiler = runExceptT . flip runStateT initState

checkName :: C.Prog -> Comp ()
checkName C.Prog{..} = f . checkDup $ varDef <> fmap g memDef
  where g (C.MemVal x _) = x
        g (C.MemList x _) = x
        f :: Bool -> Comp ()
        f True = throwError DuplicateName
        f False = pure ()

buildVars :: [C.VarDef] -> Comp ()
buildVars xs | len > 15 = throwError TooManyVariables
             | otherwise = vars .= P.fromList (go M.R5 xs)
  where go :: M.Reg -> [C.VarDef] -> [(Text, M.Reg)]
        go _ [] = []
        go r (name:xs) = (name, r) : go (succ r) xs
        len = length xs

buildMem :: [C.MemDef] -> Comp [Asm]
buildMem = foldr (liftA2 (++) . buildMemDef) (return [])

buildMemDef :: C.MemDef -> Comp [Asm]
buildMemDef (C.MemVal name val) = do
  t <- getTag
  memTag . at name .= Just t
  return [ATagged t $ Dw (VInt val)]
buildMemDef (C.MemList name (x:xs)) = do
  t <- getTag
  memTag . at name .= Just t
  return $ ATagged t (Dw $ VInt x) : (AInst . Dw . VInt <$> xs)

compileExpr :: C.Expr -> Comp [Asm]
compileExpr (C.EIdent n) = pure <$> getIdent n
compileExpr (C.ENum x) = pure . AInst . flip Lwi (VInt x) <$> use baseReg
compileExpr (C.EPlus e1 e2) = mkBinOpAsm Add e1 e2
compileExpr (C.EMinus e1 e2) = mkBinOpAsm Sub e1 e2
compileExpr (C.EAnd e1 e2) = mkBinOpAsm And e1 e2
compileExpr (C.EOr e1 e2) = mkBinOpAsm Or e1 e2
compileExpr (C.ELt e1 e2) = mkBinOpAsm Slt e1 e2
compileExpr (C.EMult e1 e2) = binOpAsm f e1 e2
  where f r1 r2 = AInst <$> [ Mult r1 r2
                            , Mflo r1
                            ]
compileExpr (C.EDiv e1 e2) = binOpAsm f e1 e2
  where f r1 r2 = AInst <$> [ Div r1 r2
                            , Mflo r1
                            ]
compileExpr (C.ENot e1) = mkUnOpAsm Not e1
compileExpr (C.ENeg e1) = mkUnOpAsm Negu e1
compileExpr (C.ERef e1) = mkUnOpAsm Lwria e1
compileExpr (C.EAddr (C.EIdent name)) = do
  base <- use baseReg
  t <- P.lookup name <$> use memTag
  case t of
    Nothing -> throwError OnlyGetAddressInMem
    Just t -> return [AInst $ Lwi base $ VAddr t]
compileExpr (C.EAddr _) = throwError OnlyGetAddressInMem

compileStmts :: C.Stmts -> Comp [Asm]
compileStmts s = mconcat <$> mapM compileStmt s

compileStmt :: C.Stmt -> Comp [Asm]
compileStmt (C.Assign name e) = compileAssign name e
compileStmt (C.While e s) = compileWhile e s
compileStmt (C.If e s1 ms2) = compileIf e s1 $ f ms2
  where f (Just x) = x
        f Nothing = []
compileStmt (C.Print e) = compilePrint e
compileStmt (C.RefAssign ea e) = compileRefAssign ea e

compileAssign :: Text -> C.Expr -> Comp [Asm]
compileAssign name e = (++) <$> compileExpr e <*> setIdent name

compileRefAssign :: C.Expr -> C.Expr -> Comp [Asm]
compileRefAssign ea e = do
  rd <- use baseReg
  a1 <- compileExpr ea
  baseReg %= succ
  rs <- use baseReg
  a2 <- compileExpr e
  baseReg %= pred
  return $ a1 ++ a2 ++ [AInst $ Swria rs rd]

compilePrint :: C.Expr -> Comp [Asm]
compilePrint e = (++ fmap AInst [Lwi M.R1 (VInt 0), Mov M.R2 M.R20, Syscall])
                 <$> compileExpr e

compileWhile :: C.Expr -> C.Stmts -> Comp [Asm]
compileWhile e s = do
  a1 <- compileExpr e
  base <- use baseReg
  tCheck <- getTag
  tEnd <- getTag
  a2 <- compileStmts s
  return $ [ATagged tCheck Nop] ++ a1 ++ [AInst $ Beq base M.R0 $ VRel tEnd]
    ++ a2 ++ [AInst . B $ VRel tCheck] ++ [ATagged tEnd Nop]

compileIf :: C.Expr -> C.Stmts -> C.Stmts -> Comp [Asm]
compileIf e s1 s2 = do
  ae <- compileExpr e
  base <- use baseReg
  tElse <- getTag
  tEnd <- getTag
  a1 <- compileStmts s1
  a2 <- compileStmts s2
  return $ ae ++ [AInst $ Beq base M.R0 $ VRel tElse]
    ++ a1 ++ [AInst . B $ VRel tEnd] ++ [ATagged tElse Nop] ++
    a2 ++ [ATagged tEnd Nop]

setIdent :: Text -> Comp [Asm]
setIdent name = P.lookup name <$> use vars >>= f
  where f :: Maybe M.Reg -> Comp [Asm]
        f Nothing = P.lookup name <$> use memTag >>= g
        f (Just r) = do
          base <- use baseReg
          return $ pure . AInst $ Mov r base
        g :: Maybe Int -> Comp [Asm]
        g Nothing = throwError $ NameUndefined name
        g (Just t) = do
          base <- use baseReg
          return $ pure . AInst $ Swia base $ VAbs t

mkBinOpAsm :: (M.Reg -> M.Reg -> M.Reg -> Inst) -> C.Expr -> C.Expr -> Comp [Asm]
mkBinOpAsm = binOpAsm . liftBinOpInst

mkUnOpAsm :: (M.Reg -> M.Reg -> Inst) -> C.Expr -> Comp [Asm]
mkUnOpAsm = unOpAsm . liftUnOpInst

binOpAsm :: (M.Reg -> M.Reg -> [Asm]) -> C.Expr -> C.Expr -> Comp [Asm]
binOpAsm f e1 e2 = do
  base <- use baseReg
  a1 <- compileExpr e1
  if base == M.R31 then throwError OutOfRegister else pure ()
  baseReg %= succ
  opr <- use baseReg
  a2 <- compileExpr e2
  baseReg %= pred
  return $ a1 ++ a2 ++ f base opr

unOpAsm :: (M.Reg -> [Asm]) -> C.Expr -> Comp [Asm]
unOpAsm f e1 = do
  a1 <- compileExpr e1
  base <- use baseReg
  return $ a1 ++ f base

liftBinOpInst :: (M.Reg -> M.Reg -> M.Reg -> Inst) -> M.Reg -> M.Reg -> [Asm]
liftBinOpInst f x y = AInst <$> [f x x y]

liftUnOpInst :: (M.Reg -> M.Reg -> Inst) -> M.Reg -> [Asm]
liftUnOpInst f x = AInst <$> [f x x]

getTag :: Comp Int
getTag = do
  t <- use tagCnt
  tagCnt %= (+1)
  return t

getIdent :: Text -> Comp Asm
getIdent name = P.lookup name <$> use vars >>= f
  where f :: Maybe M.Reg -> Comp Asm
        f Nothing = P.lookup name <$> use memTag >>= g
        f (Just x) = do
          base <- use baseReg
          return . AInst $ Mov base x
        g :: Maybe Int -> Comp Asm
        g Nothing = throwError $ NameUndefined name
        g (Just t) = do
          base <- use baseReg
          return . AInst $ Lwda base $ VAbs t

checkDup :: [Text] -> Bool
checkDup xs = length (nub xs) /= length xs

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing = error "From nothing results in nothing"
