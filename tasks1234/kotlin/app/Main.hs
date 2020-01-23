{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Main where
import Ast
import Parsers
import Interpreter
import Text.Parsec(parse)
import Text.Pretty.Simple (pPrint)

runProgram :: (Show a) => Either a Class -> IO()
runProgram (Right cl) = interpretProgram cl
runProgram (Left error) = pPrint error

main :: IO ()
main = do
    putStrLn "Write 't' to launch translator to jvm, 'i' to launch interpretator"
    option <- getLine 
    case option of
        "t" -> do
            --startTranslator
            putStrLn "Translator finished"
        "i" -> do       
            program <- readFile "test/test_program.kt"
            putStrLn $ removeComments program 0
            ast <- return $ parse parseProgram "" $ removeComments program 0
            pPrint ast
            runProgram ast
        _ -> putStrLn "Wrong option"    
