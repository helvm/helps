module HelVM.HelPS.HS2Lazy.Facade where

import           HelVM.HelPS.HS2Lazy.Builtin (expandBltin)

import           HelVM.HelIO.Control.Safe

import           HS2Lazy.Compiler            (expandCon, programToExpr, skiCompile)
import           HS2Lazy.Optimizer           (optimizeExpr)
import           HS2Lazy.PatComp             (compilePatternMatch)
import qualified HS2Lazy.Static              as Static
import qualified HS2Lazy.Type                as Type

import qualified HS2Lazy.Lexer               as Lexer
import qualified HS2Lazy.Parser              as Parser
import           HS2Lazy.Syntax


import           Data.Char                   (toLower)

run :: MonadSafe m => String -> m String
run source = insertNewline 80 . map toLower . show <$> compile source

compile :: MonadSafe m => String -> m SKI
compile source = compile' =<< analyze source where
  compile' (program, impls, classEnv, assumps) = expandBltin $ skiCompile $ optimizeExpr $ expandCon $ programToExpr $ compilePatternMatch $ ([] , [impls]) : program' where
    (_, program') = Type.tiProgram classEnv assumpsPlus program
    assumpsPlus = assumps ++ Type.preludeAssumptions

analyze :: MonadSafe m => String -> m (Program, [Impl], ClassEnv, [Assump])
analyze source = analyze' <$> classEnvMaybe where
  classEnvMaybe = liftMaybeOrError "analyze" $ Type.addCoreClasses Type.initialEnv
  analyze' classEnv = Static.analyze classEnv topDecls
  topDecls = Parser.Decl (Parser.VarDecl ("@main", [], Parser.Rhs (Parser.Var "main") [])) : Parser.parse (Lexer.lexer "argf" source)

insertNewline :: Int -> String -> String
insertNewline _ []     = []
insertNewline n source = line ++ '\n' : insertNewline n source' where (line, source') = splitAt n source
