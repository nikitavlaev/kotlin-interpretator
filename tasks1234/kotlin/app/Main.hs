module Main where

    import Ast
    import Parsers
    import Text.Parsec(parseTest)

    main :: IO ()
    main = do
        putStrLn "Start"
        program <- readFile "test/test_program.kt"
        parseTest parseProgram $ removeComments program 0
