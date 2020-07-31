module Main where
import Options.Applicative -- 解析命令行参数
import Model.Simple -- 模型机实现

data AppConfig = AppConfig { path :: String -- ^ 测试文件的路径
                           , intePath :: String -- ^ 中断服务文件的路径
                           , debug :: Int   -- ^ 调试开关，若为调试状态，则在执行完之后打印虚拟机状态
                           }

configP :: Parser AppConfig
configP = AppConfig <$> pathP <*> intePathP <*> debugP
  where pathP = strOption $ long "source"
                <> short 's'
                <> metavar "SOURCE"
                <> help "Source file path"
        intePathP = strOption $ long "service-source"
                    <> short 'e'
                    <> metavar "SOURCE"
                    <> help "Exception service source file path"
        debugP = option auto $ long "debug"
                 <> short 'd'
                 <> metavar "0 or 1"
                 <> showDefault
                 <> value 0
                 <> help "Run in debug mode"

parseConfig :: IO AppConfig
parseConfig = execParser opts
  where opts = info (configP <**> helper) $ fullDesc
          <> progDesc "A simple simulator"
          <> header "model - A Simple Simulator"

main :: IO ()
main = do
  (AppConfig p pe d) <- parseConfig -- 读取命令行参数
  if d == 0 then runFile p pe else traceFile p pe >>= print
