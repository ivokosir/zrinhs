module Main where

import CodeGenerator
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B
import Error
import Options.Applicative
import Parser
import qualified Prettify.CodeGenerator
import qualified Prettify.Parser
import qualified Prettify.TypeChecker
import Resolver
import System.IO
import TypeChecker

data Data = Data
  { sourceContents :: String,
    writeFunction :: B.ByteString -> IO (),
    logFunction :: String -> IO ()
  }

getData :: IO Data
getData = do
  (inputFilePath, outputFilePath, verbose) <- execParser optionParser

  let log =
        if verbose
          then hPutStrLn stderr
          else return . const ()

  source <-
    if inputFilePath == "-"
      then getContents
      else readFile inputFilePath

  let write =
        if outputFilePath == "-"
          then B.putStr
          else B.writeFile outputFilePath

  return (Data source write log)
  where
    options =
      (,,)
        <$> strOption
          ( long "input"
              <> short 'i'
              <> metavar "INPUT"
              <> help "Input zrin source file"
          )
        <*> strOption
          ( long "output"
              <> short 'o'
              <> metavar "OUTPUT"
              <> help "Output json file"
              <> value "-"
          )
        <*> switch
          ( long "verbose"
              <> short 'v'
              <> help "Prints output of compiler steps to stderr"
          )

    description =
      fullDesc
        <> progDesc "Zrin compiler that compiles to json file that should be consumed by zrin-llvm"
        <> header "Zrin compiler - a compiler for zrin programming language"

    optionParser = info (options <**> helper) description

compileFile :: String -> IO ()
compileFile fname = do
  (Data source write log) <- getData

  log "\n\n=====  Input  =====\n"
  log source
  case parse fname source of
    Right ast -> do
      log "\n\n=====  Parse  =====\n"
      log (Prettify.Parser.prettify ast)

      case resolve ast of
        (Right ast) -> do
          log "\n\n===== Resolve =====\n"
          log (Prettify.Parser.prettify ast)

          case checkTypes ast of
            (Right ast) -> do
              log "\n\n=====  Type   =====\n"
              log (Prettify.TypeChecker.prettify ast)

              let code = generateCode ast

              log "\n\n=====  Code   =====\n"
              log (Prettify.CodeGenerator.prettify code)

              write (encode code)
            (Left errors) -> printErrors source errors
        (Left errors) -> printErrors source errors
    Left error -> printParseError source error

main :: IO ()
main = compileFile "test2.zrin"
