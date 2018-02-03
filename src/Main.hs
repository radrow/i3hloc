{-#LANGUAGE MultiWayIf#-}

module Main(main) where

import qualified Data.Text.IO as TIO
import Control.Concurrent
import Control.Monad
import System.Clock
import System.Directory
import System.Environment(getArgs)
import Text.Trifecta(parseString, Result(Success, Failure))

import Hloc
import Config
import Parsing.ConfigParser

-- |Function used for repetition of IO action
for :: Int -> IO a -> IO ()
for i f = unless (i == 0) $
          f >> for (i-1) f

-- |Place where config is stored by default
defaultConfigPath :: IO FilePath
defaultConfigPath = (++"/.config/i3hloc/config.rcf") <$> getHomeDirectory

-- |Program configuration from arguments
data ArgsData = ArgsData { configFile :: String
                         , printDefaultConfig :: Bool
                         , printHelp :: Bool
                         , installConfig :: Bool
                         , useDefaultConfig :: Bool
                         }
-- |Empty configuration
newArgsData :: ArgsData
newArgsData = ArgsData { configFile = undefined
                       , printHelp = False
                       , printDefaultConfig = False
                       , installConfig = False
                       , useDefaultConfig = False
                       }

-- |Default configuration - has to be IO because of homedir
getDefaultArgsData :: IO ArgsData
getDefaultArgsData = defaultConfigPath >>= \loc ->
  return newArgsData {configFile = loc}

-- |Applies arguments list to accumulated ArgsData
updateArgsData :: [String] -> ArgsData -> ArgsData
updateArgsData l acc =
  case l of
    [] -> acc
    "-h":_ -> acc {printHelp = True}
    "--help":_ -> acc {printHelp = True}
    "--printDefaultConfig":_ -> acc {printDefaultConfig = True}
    "-i":t -> updateArgsData t acc {installConfig = True}
    "-c":conf:t -> updateArgsData t acc{configFile = conf}
    "-d":t -> updateArgsData t acc {useDefaultConfig = True}
    _ -> newArgsData {printHelp = True}

-- |Help to be printed on -h flag
help :: String
help = unlines
  [ "i3hloc - streamer of JSON data for i3bar"
  , ""
  , "Options:"
  , "-h --help - displays this help"
  , "--printDefaultConfig - prints default config to stdin"
  , "-i - installs used config in default directory (.config/i3hloc)"
  , "-c <FILE> uses FILE as config"
  , "-d uses default configuration"
  ]

-- |Default configuration file
defaultConfig :: String
defaultConfig = unlines
  [ "# This is example config file. Some variables are unset you may add them yourself."
  , "# Everything is statically weak typed (eg. you may pass bool value to string type,"
  , "# but not use string type where program expects int)"
  , ""
  , "# Main section"
  , "[main]"
  , "# Order of blocks"
  , "list:string blocks = {currentWindow,light,volume,wifi,eth,battery,date,hour}"
  , "int period = 1000000"
  , "int repeats = 20"
  , ""
  , "[wifi]"
  , "string type = bandwidth"
  , "string interface = wlp6s0"
  , "float period = 2"
  , "string prefix = \"|\61931 |\""
  , ""
  , "[eth]"
  , "string type = bandwidth"
  , "string interface = enp7s0"
  , "float period = 2"
  , "string prefix = \61672 "
  , ""
  , "# Requires xdotool installed"
  , "[currentWindow]"
  , "string type = command"
  , "string iomode = stdout"
  , "string command = xdotool"
  , "list:string args = {\"|getactivewindow|\",\"|getwindowname|\"}"
  , "string underline = single"
  , "string color = \"|#ffff00|\""
  , ""
  , "# script may be adjusted to user's system"
  , "[light]"
  , "string type = command"
  , "string command = bash"
  , "string stdin = \"|"
  , "t=$(light)"
  , "echo ${t::-4}"
  , "|\""
  , "string iomode = stdout"
  , "string prefix = \61829 "
  , ""
  , "# pulseaudio volume"
  , "[volume]"
  , "string type = volume"
  , "string color = \"|#ffff22|\""
  , ""
  , "# requires acpi installed"
  , "[battery]"
  , "string type = battery"
  , ""
  , "[date]"
  , "string type = time"
  , "string format = %d-%M-%Y"
  , "string color = \"|#ccffcc|\""
  , "string prefix = \61555 "
  , ""
  , "[hour]"
  , "string type = time"
  , "string format = %H:%m:%s"
  , "string color = \"|#ccccff|\""
  , "string prefix = \61463"
  ]

-- |Reads config file as string or fails
getConfigString :: ArgsData -> IO (Either String String)
getConfigString args = if useDefaultConfig args then return $ Right defaultConfig
  else do
    configFileExists <- doesFileExist (configFile args)
    if not configFileExists
      then return $ Left "Selected config file does not exist. Generate new using \"--printDefaultConfig\" flag."
      else Right <$> readFile (configFile args)

-- |Installs used config string in selected file
installConfigInDir :: String -> FilePath -> IO ()
installConfigInDir confString fp = do
  let dir = reverse . dropWhile (/= '/') . reverse $ fp
  createDirectoryIfMissing True dir
  putStrLn $ "Installing config in " ++ dir
  writeFile fp confString

-- |Main loop of program. Runs i3hloc with selected Config
loopWithConfig :: Config -> IO()
loopWithConfig conf = do
  TIO.putStrLn jsonInit
  forever $ do
    preTime <- (`div`1000) . toNanoSecs <$> getTime Monotonic
    statusList <- getBarText conf
    for (printRepeats conf) (TIO.putStrLn statusList)
    postTime <- (`div`1000) . toNanoSecs <$> getTime Monotonic

    let deltaTime = max (postTime - preTime) 10
    threadDelay . fromIntegral $ (updatePeriod conf - deltaTime)

main :: IO ()
main = do
  settings <- getArgs >>= \l -> updateArgsData l <$> getDefaultArgsData
  if | printHelp settings -> putStr help
     | printDefaultConfig settings -> putStr defaultConfig
     | otherwise -> do
         readResult <- getConfigString settings
         case readResult of
           Left err -> putStrLn err
           Right str ->
             if installConfig settings
               then installConfigInDir str (configFile settings)
               else case parseString parseConfigFile mempty str of
                      Failure f -> print f
                      Success c -> loopWithConfig c
