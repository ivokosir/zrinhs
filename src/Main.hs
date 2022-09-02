module Main where

import CodeGenerator
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
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
  { fileName :: String,
    sourceContents :: String,
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

  return (Data inputFilePath source write log)
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

main :: IO ()
main = do
  (Data fname source write log) <- getData

  log "\n\n=====  Input  =====\n"
  log source

  log <- return (lift . log)
  let runPasses = do
        ast <- except (parse fname source)
        log "\n\n=====  Parse  =====\n"
        log (Prettify.Parser.prettify ast)

        ast <- except (resolve ast)
        log "\n\n===== Resolve =====\n"
        log (Prettify.Parser.prettify ast)

        ast <- except (checkTypes ast)
        log "\n\n=====  Type   =====\n"
        log (Prettify.TypeChecker.prettify ast)

        let code = generateCode ast
        log "\n\n=====  Code   =====\n"
        log (Prettify.CodeGenerator.prettify code)

        return code

  result <- runExceptT runPasses

  case result of
    Right code -> write (encode code)
    Left errors -> printErrors source errors
