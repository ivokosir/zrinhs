module Error (Error (..), printParseError, printErrors) where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Text.Parsec.Error
import Text.Parsec.Pos

data Error = Error SourcePos SourcePos String deriving (Eq, Show)

resetColor = "\x1b[0m"

dim = "\x1b[0;2m"

red = "\x1b[0;31m"

lightCyan = "\x1b[0;96m"

colorErrorLine :: Maybe Int -> Maybe Int -> String -> String
colorErrorLine start end line =
  let (init, right) = case end of
        Just i -> splitAt (i - 1) line
        Nothing -> (line, [])
      (left, center) = case start of
        Just i -> splitAt (i - 1) init
        Nothing -> ([], line)
   in left ++ red ++ center ++ resetColor ++ right

contextSize = 1

getLines :: SourcePos -> SourcePos -> String -> String
getLines start end source =
  let indexedLines = zip (lines source) [1 ..]

      isInContext num =
        num
          >= sourceLine start
          - contextSize
          && num
          <= sourceLine end
          + contextSize
      isErrorLine num = num >= sourceLine start && num <= sourceLine end
      errorLineStart lineNum =
        if lineNum == sourceLine start then Just (sourceColumn start) else Nothing
      errorLineEnd lineNum =
        if lineNum == sourceLine end then Just (sourceColumn end) else Nothing

      f (line, num) =
        if isInContext num
          then
            Just $
              if isErrorLine num
                then colorErrorLine (errorLineStart num) (errorLineEnd num) line
                else dim ++ line ++ resetColor
          else Nothing

      locationText =
        lightCyan
          ++ sourceName start
          ++ " "
          ++ show (sourceLine start)
          ++ ":"
          ++ show (sourceColumn start)
          ++ resetColor
   in locationText ++ "\n" ++ intercalate "\n" (mapMaybe f indexedLines)

formatParseError :: String -> ParseError -> String
formatParseError source error =
  let start = errorPos error
      end = setSourceColumn start (sourceColumn start + 1)
      message =
        showErrorMessages
          "or"
          "unknown parse error"
          "expecting"
          "unexpected"
          "end of input"
          (errorMessages error)
   in getLines start end source ++ message

printParseError :: String -> ParseError -> IO ()
printParseError source error = putStrLn $ formatParseError source error

formatError :: String -> Error -> String
formatError source (Error start end error) =
  getLines start end source ++ "\n" ++ error

printErrors :: String -> [Error] -> IO ()
printErrors source errors =
  putStrLn $ intercalate "\n" $ fmap (formatError source) errors
