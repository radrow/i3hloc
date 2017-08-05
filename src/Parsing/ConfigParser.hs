module Parsing.ConfigParser where

import Control.Applicative
import Control.Monad.State
import Data.Char(toLower)
import Data.List(find)
import Data.Map.Strict(Map, lookup)
import Data.Maybe(fromJust, fromMaybe, isJust)
import qualified Data.Map as M
import Data.Text(pack)
import Text.Trifecta
import Web.FontAwesomeType

import Block
import Blocks.Bandwidth
import Blocks.Battery
import Blocks.Command
import Blocks.Volume
import Blocks.Time
import DisplayText
import Colors
import Pango

-- |Comment used in rcf "language"
comment :: String
comment = "#"

-- |Map lookup. Compiler could not import it, so made my own version
(!?) :: Ord a => Map a b -> a -> Maybe b
(!?) = flip Data.Map.Strict.lookup

-- types
-- |Header of section
newtype Header = Header String
-- |Name of field
type Name = String
-- |Packed types used in rcf file
data HType = AInt Integer
           | AFloat Double
           | AString String
           | ABool Bool
           | AChar Char
           | AList [HType]
           deriving (Eq, Show)
-- |Map from names to values
type AssignmentsMap = Map String HType
-- |Section similar to those known from .ini files - contains header and fields
data BlockSection = Section Header AssignmentsMap

-- types extraction
hTypeToString :: HType -> String
hTypeToString h = case h of
  AInt i -> show i
  AFloat f -> show f
  AString s -> s
  ABool b -> show b
  AChar c -> [c]
  AList l -> show (map hTypeToString l)

-- |Unpacks value assuming it's an Integer
getInteger :: HType -> Maybe Integer
getInteger (AInt i) = Just i
getInteger _ = Nothing

-- |Unpacks value assuming it's a Double
getDouble :: HType -> Maybe Double
getDouble (AFloat d) = Just d
getDouble _ = Nothing

-- |Unpacks value assuming it's a Char
getChar :: HType -> Maybe Char
getChar (AChar i) = Just i
getChar _ = Nothing

-- |Unpacks value assuming it's a Bool
getBool :: HType -> Maybe Bool
getBool (ABool b) = Just b
getBool _ = Nothing

-- |Unpacks value assuming it's a String
getString :: HType -> Maybe String
getString (AString s) = Just s
getString _ = Nothing

-- |Unpacks value assuming it's a list
getList :: HType -> Maybe [HType]
getList (AList l) = Just l
getList _ = Nothing

-- skips
skipEOL :: Parser ()
skipEOL = skipMany . char $ '\n'

skipComments :: Parser ()
skipComments = skipMany $ do _ <- string comment
                             skipMany (noneOf "\n")
                             skipEOL
skipWhitespaces :: Parser ()
skipWhitespaces = skipMany $ char ' ' <|> char '\n'

skipBreaks :: Parser ()
skipBreaks = skipWhitespaces >> skipComments >> skipWhitespaces

-- misc
-- |Parses string that is surrounded by some other strings
surrounded :: String -> String -> Parser a -> Parser a
surrounded b e p = string b *> p <* string e

-- parsing
-- |Parses int
parseAInt :: Parser HType
parseAInt = AInt <$> integer

-- |Parses float
parseAFloat :: Parser HType
parseAFloat = AFloat <$> ((fromInteger <$> integer) <|> double)

-- |Parses bool
parseABool :: Parser HType
parseABool = ABool <$> do
  b <- map toLower
    <$> string "true"
    <|> string "false"
    <|> string "True"
    <|> string "False"
  return $ b == "true"

-- |Parses string. As default surround uses "| |"
parseAString :: Parser HType
parseAString = AString <$> (surrounded "\"|" "" (manyTill anyChar (string "|\"")) <|> some (noneOf "\n"))

-- |Parses char
parseAChar :: Parser HType
parseAChar = AChar <$> surrounded "'" "'" anyChar

-- |Parses header
parseHeader :: Parser Header
parseHeader = surrounded "[" "]" (Header <$> some letter)

-- |Parses list of some expressions
parseList :: Parser a -> Parser [a]
parseList p = do
  _ <- char '{'
  (char '}' >> return []) <|> do
    elm <- p
    let parse acc = do
          skipWhitespaces
          (char '}' >> return acc) <|>
            do _ <- char ','
               skipWhitespaces
               e <- p
               parse (e:acc)
    reverse <$> parse [elm]

-- |Returns adequate parser by type name
hTypeParserFromString :: String -> Parser HType
hTypeParserFromString s = case s of
  "string" -> parseAString
  "int" -> parseAInt
  "float" -> parseAFloat
  "char" -> parseAChar
  "bool" -> parseABool
  'l':'i':'s':'t':':':k -> AList <$> parseList (hTypeParserFromString k)
  _ -> fail "Unknown type"

-- |Parses variable assignment
parseAssignment :: Parser (Name, HType)
parseAssignment = do
  let parse :: Parser (String, HType)
      parse = do
        typename <- some (alphaNum <|> char ':')
        skipMany (char ' ')
        n <- some alphaNum
        _ <- skipMany (char ' ') >> char '=' >> skipMany (char ' ')
        h <- hTypeParserFromString typename
        return (n, h)
  (name, val) <- parse
  skipEOL
  return (name, val)

-- |Parses whole section
parseBlockSection :: Parser BlockSection
parseBlockSection = do
  skipBreaks
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

-- |Converts parsed section to working block. Returns errorBlock on fail
sectionToBlock :: BlockSection -> Block
sectionToBlock (Section _ assg) =
  let mdisplayType = assg !? "type"
      mcolor = assg !? "color"
      mbgColor = assg !? "bgcolor"
      mprefix = assg !? "prefix"
      msuffix = assg !? "suffix"
      munderline = assg !? "underline"
      maybeDo :: Applicative f => Maybe a -> f () -> f()
      maybeDo k = when $ isJust k
  in case mdisplayType >>= getString of
      Nothing -> errorBlock "Missing string \"type\" field"
      Just dtname -> case createBlockDisplayText dtname assg of
        Left e -> errorBlock e
        Right dt -> flip execState (makeBlock dt) $ do
          maybeDo mcolor $ do
            let col = fromJust mcolor
            b <- get
            case col of
              AString s -> put $ b {color = fromMaybe pink (makeColor s)}
              _ -> put b
          maybeDo mbgColor $ do
            let bgcol = fromJust mbgColor
            b <- get
            case bgcol of
              AString s -> put $ b {bgColor = makeColor s}
              _ -> put b
          maybeDo mprefix $ do
            let pref = fromJust mprefix
            b <- get
            put $ b {prefix = pack $ hTypeToString pref}
          maybeDo msuffix $ do
            let suf = fromJust msuffix
            b <- get
            put $ b {suffix = pack $ hTypeToString suf}
          maybeDo munderline $ do
            let und = fromJust munderline
            b <- get
            case und of
              AString s -> let us = underlineModeFromString s
                in case us of Just uss -> put $ b {underline = uss}
                              Nothing -> put b
              _ -> put b
          get

-- |Type used for error handling
type ErrMsg = String

-- |Creates DisplayText of certain type using given values
createBlockDisplayText :: String -> AssignmentsMap -> Either ErrMsg DisplayText
createBlockDisplayText name fields =
  let getValue :: String -> Either ErrMsg HType
      getValue s = case fields !? s of
        Just ss -> Right ss
        Nothing -> Left $ show s ++ " value is missing."
      explainMaybe :: String -> Maybe a -> Either ErrMsg a
      explainMaybe _ (Just a) = Right a
      explainMaybe e Nothing = Left e
      result :: Display d => IO d -> Either ErrMsg DisplayText
      result = return . display
  in case name of
    "bandwidth" -> do
      interfacePacked <- getValue "interface"
      periodPacked <- getValue "period"
      interface <- explainMaybe "Invalid type for interface" $ getString interfacePacked
      period <- explainMaybe "Invalid type for period" $ getDouble periodPacked
      result $ getInterfaceFullInfo period interface

    "battery" -> result getBatteryState

    "time" -> do
      formatP <- getValue "format"
      format <- explainMaybe "Invalid type for format" $ getString formatP
      result $ getTime (reverse (parseFormat [] format)) where
        parseFormat :: [Seq] -> String -> [Seq]
        parseFormat acc [] = acc
        parseFormat acc ('%':k:t) = flip parseFormat t $
                                  (:) (fromMaybe (Separator "") (readSeq k)) acc
        parseFormat acc (h:t) = parseFormat (Separator [h]:acc) t

    "command" -> do
      let iomodeP = fromMaybe (AString "stdout") (fields !? "iomode")
          argsP = fromMaybe (AList []) (fields !? "args")
          stdinP = fromMaybe (AString "") (fields !? "stdin")
      iomode <- explainMaybe "Invalid type for iomode" $ getString iomodeP
      commandP <- getValue "command"
      command <- explainMaybe "command must be string" $ getString commandP
      stdin <- explainMaybe "stdin must be string" $ getString stdinP
      args <- case argsP of
               AString s -> return [s]
               AList l -> explainMaybe "Every arg must be a string" .
                 mapM getString $ l
               _ -> Left "Invalid type for args"
      case iomode of "stdout" -> result $ customCommandOut command args stdin
                     "stderr" -> result $ customCommandErr command args stdin
                     "retcode" -> result $ customCommandRet command args stdin
                     _ -> result $ customCommandOut command args stdin

    "volume" -> result $ do
      mute <- isMute
      vol <- getVolume
      let out | mute = [fa FaVolumeOff, fa FaTimes]
              | vol < 15 = fa FaVolumeOff : ' ' : show vol
              | vol < 65 = fa FaVolumeDown : ' ' : show vol
              | otherwise = fa FaVolumeUp : ' ' : show vol
            where fa = fontAwesomeChar
      return out

    _ -> Left $ "Unknown block kind: '" ++ name ++ "'"

-- |Parses whole configuration file
parseConfigFile :: Parser [Block]
parseConfigFile = do
  skipBreaks
  blockList <- parseList (some letter)
  sections <- some parseBlockSection
  return $ flip map blockList $ \name ->
    case find (\(Section (Header h) _) -> h == name) sections of
      Nothing -> errorBlock $ "No such section: '" ++ name ++ "'"
      Just s -> sectionToBlock s
