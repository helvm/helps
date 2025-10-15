module HelVM.HelPS.HS2Lazy.Facade where

import           HelVM.HelPS.HS2Lazy.Builtin                         (expandBltin)
import           HelVM.HelPS.HS2Lazy.Compiler.ExpandCon              (expandCon)
import           HelVM.HelPS.HS2Lazy.Compiler.SkiCompiler            (skiCompile)
import           HelVM.HelPS.HS2Lazy.Optimizer                       (optimizeExpr)

import           HelVM.HelIO.Control.Safe

import           HelVM.HelPS.HS2Lazy.Compiler.ProgramToExprConverter (programToExpr)
import           HelVM.HelPS.HS2Lazy.PatComp.PatternMatchCompiler    (compilePatternMatch)
import qualified HS2Lazy.Static                                      as Static
import qualified HS2Lazy.Type                                        as Type

import qualified HS2Lazy.Lexer                                       as Lexer
import qualified HS2Lazy.Parser                                      as Parser
import           HS2Lazy.Syntax

import           Data.Char                                           (toLower)

run :: MonadSafe m => String -> m String
run source = insertNewline 80 . map toLower . show <$> compile source

compile :: MonadSafe m => String -> m SKI
compile source = compile' =<< analyze source where
  compile' (program, impls, classEnv, assumps) = expandBltin =<< skiCompiled where
    skiCompiled = skiCompile =<< optimizedExpr
    optimizedExpr = optimizeExpr =<< expandedCon
    expandedCon = expandCon =<< expr
    expr = programToExpr $ compilePatternMatch $ ([] , [impls]) : program'
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
