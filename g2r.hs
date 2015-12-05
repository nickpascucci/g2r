{-# LANGUAGE OverloadedStrings #-}

import Text.Parsec
import Data.Time
import Data.Word
import System.Environment (getArgs)

data Coordinate = XYZ Float Float Float
                | IJK Float Float Float
                | ABC Float Float Float
                deriving Show

data Plane = XY | XZ | YZ deriving Show
data Unit = Millimeters | Inches deriving Show
data Direction = Clockwise | CounterClockwise deriving Show
data CoordinateSystem = Machine
                      | Workpiece Float Float Float
                      deriving Show

data DistanceMode = Absolute | Relative deriving Show
data FeedMode = InverseTime | UnitsPerMinute | UnitsPerRevolution deriving Show

-- GCode command
data Command = RapidMove Coordinate -- G00
             | CoordinatedMove Coordinate -- G1
             | Arc Direction Coordinate Coordinate Float -- G2/G3
             | ToolChange Integer -- T*
             | SpindleSpeed Integer -- S*
             | PlaneSelect Plane -- G17/G18/G19
             | UnitSelect Unit -- G20/G21
             | Home -- G28
             | ToolLengthCompensate Integer -- G43
             | CoordinateSystemSelect CoordinateSystem -- G53-G59
             | DistanceModeSelect DistanceMode -- G90/G91
             | FeedModeSelect FeedMode -- G93/G94/G95
             deriving Show

-- Line number and command
data ProgramLine = ProgramLine {
  lineNumber :: Integer,
  commands :: [Command]
  } deriving Show

type Program = [ProgramLine]

commandParser :: Parsec String () Command
commandParser = do
  char 'G'
  n <- many1 digit
  return $ case (read n) of
    28 -> Home

-- Matches n > 0 space characters.
spacesOnly :: Parsec String () String
spacesOnly = do
  spaces <- many1 (char ' ')
  return spaces

lineParser :: Parsec String () ProgramLine
lineParser = do
  char 'N'
  n <- many1 digit
  cmds <- many1 (spacesOnly *> commandParser)
  return $ ProgramLine (read n) cmds

programParser :: Parsec String () Program
programParser = many $ lineParser <* endOfLine

main :: IO ()
main = do
  args <- getArgs
  case args of
    [logFile] -> readFile logFile >>= parseTest programParser
    _ -> putStrLn "Error: input file required"
