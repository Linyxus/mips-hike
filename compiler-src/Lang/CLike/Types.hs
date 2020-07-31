module Lang.CLike.Types where
import Data.Text (Text)

data Prog = Prog { varDef :: [VarDef]
                 , memDef :: [MemDef]
                 , body :: Stmts
                 } deriving (Eq, Show)

type Ident = Text

type VarDef = Ident

data MemDef = MemVal Text Int
            | MemList Text [Int]
            deriving (Eq, Show)

data Stmt = Assign Ident Expr
          | RefAssign Expr Expr
          | While Expr Stmts
          | If Expr Stmts (Maybe Stmts)
          | Print Expr
          deriving (Eq, Show)

type Stmts = [Stmt]

data Expr = EPlus Expr Expr
          | EMinus Expr Expr
          | EMult Expr Expr
          | EDiv Expr Expr
          | EAnd Expr Expr
          | EOr Expr Expr
          | ENot Expr
          | ENeg Expr
          | ERef Expr
          | EAddr Expr
          | EGt Expr Expr
          | ELt Expr Expr
          | EGe Expr Expr
          | ELe Expr Expr
          | EEq Expr Expr
          | ENeq Expr Expr
          | EIdent Text
          | ENum Int
          deriving (Eq, Show)
