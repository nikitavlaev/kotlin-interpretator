module Main where

    import Ast
    import Parsers
    import Text.Parsec(parse)
    import Text.Pretty.Simple (pPrint)

    main :: IO ()
    main = do
        putStrLn "Start"
        program <- readFile "test/test_program.kt"
        putStrLn $ removeComments program 0
        pPrint $ parse parseProgram "" $ removeComments program 0
