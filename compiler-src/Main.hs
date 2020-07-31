module Main where
import Options.Applicative
import Lang.Compiler.Compile

data AppConfig = AppConfig { path :: String -- ^ 测试文件的路径
                           , output :: String -- ^ 输出路径
                           , outAsm :: Bool -- ^ 是否输出中间汇编代码
                           }

configP :: Parser AppConfig
configP = AppConfig <$> pathP <*> outputP <*> asmP
  where pathP = strOption $ long "source"
                <> short 's'
                <> metavar "SOURCE"
                <> help "Source file path"
        outputP = strOption $ long "output"
                  <> short 'o'
                  <> metavar "OUTPUT"
                  <> help "Output file path"
        asmP = switch $ long "asm"
               <> short 'a'
               <> help "Output assembly source"

parseConfig :: IO AppConfig
parseConfig = execParser opts
  where opts = info (configP <**> helper) $ fullDesc
          <> progDesc "A simple simulator"
          <> header "model - A Simple Simulator"

main :: IO ()
main = do
  (AppConfig p o a) <- parseConfig -- 读取命令行参数
  (if a then compileFileAsm else compileFile) p >>= g o
  where g _ (Left e) = print e
        g o (Right x) = writeFile o x
