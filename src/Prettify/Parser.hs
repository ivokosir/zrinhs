module Prettify.Parser (prettify) where

import AST hiding (Expression)
import Data.List (intercalate)
import Parser

prettifyOperation :: Operation -> String
prettifyOperation Or = "or"
prettifyOperation And = "and"
prettifyOperation Equal = "=="
prettifyOperation NotEqual = "!="
prettifyOperation Greater = ">"
prettifyOperation GreaterEqual = ">="
prettifyOperation Less = "<"
prettifyOperation LessEqual = "<="
prettifyOperation Add = "+"
prettifyOperation Subtract = "-"
prettifyOperation Multiply = "*"
prettifyOperation Divide = "/"
prettifyOperation Remainder = "%"

prettifyConst :: Constant -> String
prettifyConst CUnit = "()"
prettifyConst (CBool True) = "true"
prettifyConst (CBool False) = "false"
prettifyConst (CInt i) = show i
prettifyConst (CString s) = "\"" ++ s ++ "\""

prettify_ :: String -> Expression -> String
prettify_ indent expression =
  let nextIndent = indent ++ "  "
      prettifySub = prettify_ nextIndent
   in case base expression of
        (Block [] e) -> "(" ++ prettifySub e ++ ")"
        (Block es e) ->
          let seperator = ";\n" ++ nextIndent
           in "(\n"
                ++ nextIndent
                ++ intercalate seperator (fmap prettifySub es)
                ++ seperator
                ++ prettifySub e
                ++ "\n"
                ++ indent
                ++ ")"
        (Definition name e) -> name ++ " = " ++ prettify_ indent e
        (IfThenElse c t e) ->
          "if\n"
            ++ nextIndent
            ++ prettifySub c
            ++ "\n"
            ++ indent
            ++ "then\n"
            ++ nextIndent
            ++ prettifySub t
            ++ "\n"
            ++ indent
            ++ "else\n"
            ++ nextIndent
            ++ prettifySub e
            ++ "\n"
            ++ indent
            ++ "end"
        (Operation op lhs rhs) ->
          prettifySub lhs
            ++ " "
            ++ prettifyOperation op
            ++ " "
            ++ prettifySub rhs
        (Function param e) -> param ++ " -> " ++ prettify_ indent e
        (Call caller arg) -> prettifySub caller ++ " " ++ prettifySub arg
        (Identifier name) -> name
        (Literal const) -> prettifyConst const

prettify :: Expression -> String
prettify = prettify_ ""
