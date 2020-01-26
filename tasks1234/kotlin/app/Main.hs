{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Main where
import Ast
import System.Environment
import Parsers
import Translator
import Interpreter
import Text.Parsec(parse)
import Text.Pretty.Simple (pPrint)
import Control.Monad.State.Lazy
import Control.Monad.Reader


runProgram :: (Show a) => Either a Class -> IO()
runProgram (Right cl) = interpretProgram cl
runProgram (Left error) = pPrint error

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("t":name:other) -> do
            {-translateHelloWorld name
            putStrLn $ "Translator finished " ++ name-}
            program <- readFile "test/test_translator.kt"
            putStrLn $ removeComments program 0
            ast <- return $ parse parseProgram "" $ removeComments program 0
            pPrint ast 
            case ast of 
                (Right mainClass@(Class _ _ (fun:funs) _ )) -> do 
                                                let result = reverse $ runReader (execStateT (evalStateT (translatorMethod fun) []) []) mainClass
                                                pPrint result
                _ -> putStrLn "Unsupported program"                                 
        ("i":other) -> do       
            program <- readFile "test/test_program.kt"
            putStrLn $ removeComments program 0
            ast <- return $ parse parseProgram "" $ removeComments program 0
            pPrint ast
            runProgram ast
        _ -> putStrLn "Wrong option"    
