module Parsing.ConfigParser where


import Control.Applicative
import Control.Monad.State
import Data.ByteString(ByteString)
import Data.Char(isAlpha, toLower)
import Data.Map.Strict(Map, (!?))
import Data.Maybe(fromJust, fromMaybe, isJust)
import qualified Data.Map as M
import Data.Text(Text, pack)
import qualified Data.Text.IO as TIO
import Text.RawString.QQ
import Text.Trifecta

import Block
import Colors
import Config
import Pango

comment :: Char
comment = '#'

-- types
newtype Header = Header String
type Name = String
data HType = AInt Integer
           | AFloat Double
           | AString String
           | ABool Bool
           | AChar Char
           deriving (Eq, Show)
type AssignmentsMap = Map String HType
data BlockSection = Section Header AssignmentsMap

hTypeToString :: HType -> String
hTypeToString h = case h of
  AInt i -> show i
  AFloat f -> show f
  AString s -> s
  ABool b -> show b
  AChar c -> [c]

-- skips
skipEOL :: Parser ()
skipEOL = skipMany . char $ '\n'

skipComments :: Parser ()
skipComments = do _ <- char comment
                  skipMany (noneOf "\n")
                  skipEOL
skipWhitespaces :: Parser ()
skipWhitespaces = skipMany $ char ' ' <|> char '\n'

-- misc
surrounded :: String -> String -> Parser a -> Parser a
surrounded b e p = string b *> p <* string e

-- parsing
parseAInt :: Parser HType
parseAInt = AInt <$> integer

parseAFloat :: Parser HType
parseAFloat = AFloat <$> (double <|> fromInteger <$> integer)

parseABool :: Parser HType
parseABool = ABool <$> do
  b <- map toLower
    <$> string "true"
    <|> string "false"
    <|> string "True"
    <|> string "False"
  return $ b == "true"

parseAString :: Parser HType
parseAString = AString <$> surrounded "\"" "\"" (some (noneOf "\""))

parseAChar :: Parser HType
parseAChar = AChar <$> surrounded "'" "'" anyChar

parseHeader :: Parser Header
parseHeader = surrounded "[" "]" (Header <$> some letter)

parseAssignment :: Parser (Name, HType)
parseAssignment = do
  let getAs :: String -> Parser HType -> Parser (String, HType)
      getAs typename pars = do
        _ <- string $ typename ++ " "
        n <- some alphaNum
        _ <- some (char ' ') >> char '=' >> some (char ' ')
        h <- pars
        return (n, h)
  (name, val) <- getAs "string" parseAString
                  <|> getAs "int" parseAInt
                  <|> getAs "float" parseAFloat
                  <|> getAs "bool" parseABool
                  <|> getAs "char" parseAChar
  skipEOL
  return (name, val)

parseBlockSection :: Parser BlockSection
parseBlockSection = do
  skipComments
  skipWhitespaces
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

sectionToBlock :: BlockSection -> Block
sectionToBlock (Section _ assg) =
  let mdisplayT = assg !? "displayText"
      mcolor = assg !? "color"
      mbgColor = assg !? "bgcolor"
      mprefix = assg !? "prefix"
      msuffix = assg !? "suffix"
      munderline = assg !? "underline"
      maybeDo :: Applicative f => Maybe a -> f () -> f()
      maybeDo k = when $ isJust k
  in flip execState newBlock $ do
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
      let pref = fromJust msuffix
      b <- get
      put $ b {suffix = pack $ hTypeToString pref}
    maybeDo munderline $ do
      let und = fromJust munderline
      b <- get
      case und of
        AString s -> let us = underlineModeFromString s
                    in case us of Just uss -> put $ b {underline = uss}
                                  Nothing -> put b
        _ -> put b


