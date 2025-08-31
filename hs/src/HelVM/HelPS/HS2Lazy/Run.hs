module HelVM.HelPS.HS2Lazy.Run where

import           HelVM.HelPS.HS2Lazy.Builtin   (expandBltin)

import           HelVM.HelPS.HS2Lazy.Compiler  (expandCon, programToExpr, skiCompile)
import           HelVM.HelPS.HS2Lazy.Optimizer (optimizeExpr)
import qualified HS2Lazy.Lexer                 as Lexer
import qualified HS2Lazy.Parser                as Parser
import           HS2Lazy.PatComp               (compilePatternMatch)
import qualified HS2Lazy.Static                as Static
import           HS2Lazy.Syntax
import qualified HS2Lazy.Type                  as Type

import           Data.Char                     (toLower)
import qualified Relude.Unsafe                 as Unsafe

run :: String -> String
run source = insertNewline 80 $ map toLower $ show $ compile source

compile :: String -> SKI
compile source = expandBltin $ skiCompile $ optimizeExpr $ expandCon $ programToExpr $ compilePatternMatch $ ([] , [impls]) : program' where
  (_, program') = Type.tiProgram classEnv assumpsPlus program
  assumpsPlus = assumps ++ Type.preludeAssumptions
  (program, impls, classEnv, assumps) = analyze source

analyze :: String -> (Program, [Impl], ClassEnv, [Assump])
analyze source = Static.analyze classEnv topDecls where
  classEnv = Unsafe.fromJust $ Type.addCoreClasses Type.initialEnv
  topDecls = Parser.Decl (Parser.VarDecl ("@main", [], Parser.Rhs (Parser.Var "main") []))  : Parser.parse (Lexer.lexer "argf" source)

insertNewline :: Int -> String -> String
insertNewline _ []     = []
insertNewline n source = line ++ '\n' : insertNewline n source' where (line, source') = splitAt n source
