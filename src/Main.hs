module Main where

import CodeGenerator
import Control.Monad.Except
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

  result <-
    runExceptT
      ( do
          let l = liftIO . log
          ast <- liftEither (parse fname source)
          l "\n\n=====  Parse  =====\n"
          l (Prettify.Parser.prettify ast)

          ast <- liftEither (resolve ast)
          l "\n\n===== Resolve =====\n"
          l (Prettify.Parser.prettify ast)

          ast <- liftEither (checkTypes ast)
          l "\n\n=====  Type   =====\n"
          l (Prettify.TypeChecker.prettify ast)

          let code = generateCode ast
          l "\n\n=====  Code   =====\n"
          l (Prettify.CodeGenerator.prettify code)

          return code
      )

  case result of
    Right code -> write (encode code)
    Left errors -> printErrors source errors

main :: IO ()
main = compileFile "test2.zrin"
