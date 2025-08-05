module HelVM.HelPS.HS2Lazy.Main where
import System.Environment
import System.IO
import HelVM.HelPS.HS2Lazy.Syntax
import qualified HelVM.HelPS.HS2Lazy.Lexer as L
import qualified HelVM.HelPS.HS2Lazy.Parser as P
import qualified HelVM.HelPS.HS2Lazy.Static as S
import qualified HelVM.HelPS.HS2Lazy.Type as T
import HelVM.HelPS.HS2Lazy.Compiler (programToExpr, expandCon, skiCompile)
import HelVM.HelPS.HS2Lazy.PatComp (compilePatternMatch)
import HelVM.HelPS.HS2Lazy.Optimizer (optimizeExpr)
import HelVM.HelPS.HS2Lazy.Builtin (expandBltin)
import PHelVM.HelPS.HS2Lazy.Print (showProgram)

compile s = (prog, as ++ a, expr2, ski2, ce')
    where (prog, is, ce', as) = S.analyze ce topdecls
          topdecls = P.Decl (P.VarDecl ("@main", [], P.Rhs (P.Var "main") []))
                     : (P.parse (L.lexer "argf" s))
          as' = as ++ T.preludeAssumptions
	  (a, prog') = T.tiProgram ce' as' prog
          prog2 = ([],[is]):prog'
          prog3 = compilePatternMatch prog2
          expr1 = expandCon $ programToExpr prog3
          expr2 = optimizeExpr expr1
          ski1 = skiCompile expr2
	  ski2 = expandBltin ski1
          Just ce = T.addCoreClasses T.initialEnv

main :: IO ()
main = do source <- argf
	  let (p, as, p', e, ce) = compile source in
	      do --hPutStrLn stderr (show p')
                 --mapM_ (hPutStrLn stderr . show) as
		 putStrLn (show e)

argf :: IO String
argf = do argv <- getArgs
	  if argv == []
	     then getContents
	     else do conts <- mapM getFileContents argv
		     return (concat conts)

getFileContents :: String -> IO String
getFileContents fname = do handle <- openFile fname ReadMode
			   hGetContents handle
