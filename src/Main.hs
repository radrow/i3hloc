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

-- period over 1 milion with current time display is reasonless
period :: Integer
period = 1000000

repeats :: Int
repeats = 20

for :: Int -> IO a -> IO ()
for i f = unless (i == 0) $
          f >> for (i-1) f

defaultConfigLoc :: IO FilePath
defaultConfigLoc = (++"/.config/i3hloc/config.rcf") <$> getHomeDirectory

data ArgsData = ArgsData { configFile :: String
                         , printDefaultConfig :: Bool
                         , printHelp :: Bool
                         , installConfig :: Bool
                         , useDefaultConfig :: Bool
                         }

newArgsData :: ArgsData
newArgsData = ArgsData { configFile = undefined
                       , printHelp = False
                       , printDefaultConfig = False
                       , installConfig = False
                       , useDefaultConfig = False
                       }

getDefaultArgsData :: IO ArgsData
getDefaultArgsData = defaultConfigLoc >>= \loc ->
  return newArgsData {configFile = loc}

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

help :: String
help = "Hi! This is help\n"

defaultConfig :: String
defaultConfig = unlines
  [ "# This is example config file. Some variables are unset you may add them yourself."
  , "# Everything is statically weak typed (eg. you may pass bool value to string type,"
  , "# but not use string type where program expects int)"
  , ""
  , "# Order of blocks"
  , "{currentWindow,light,volume,wifi,eth,battery,date,hour}"
  , ""
  , "[wifi]"
  , "string type = bandwidth"
  , "string interface = wlp6s0"
  , "float period = 2"
  , "string prefix = \"|\61931 |\""
  , ""
  , "eth]"
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

loopWithConfig :: Config -> IO()
loopWithConfig conf = do
  TIO.putStrLn jsonInit
  forever $ do
    preTime <- (`div`1000) . toNanoSecs <$> getTime Monotonic
    statusList <- getBarText conf
    for repeats (TIO.putStrLn statusList)
    postTime <- (`div`1000) . toNanoSecs <$> getTime Monotonic

    let deltaTime = max (postTime - preTime) 10
    threadDelay . fromIntegral $ (period - deltaTime)


getConfigString :: ArgsData -> IO (Either String String)
getConfigString args = if useDefaultConfig args then return $ Right defaultConfig
  else do
    configFileExists <- doesFileExist (configFile args)
    if not configFileExists
      then return $ Left "Selected config file does not exist. Generate new using \"--printDefaultConfig\" flag."
      else Right <$> readFile (configFile args)


installConfigInDir :: String -> FilePath -> IO ()
installConfigInDir confString fp = do
  let dir = reverse . dropWhile (/= '/') . reverse $ fp
  createDirectoryIfMissing True dir
  putStrLn $ "Installing config in " ++ dir
  writeFile fp confString

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
