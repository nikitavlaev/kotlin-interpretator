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
        putStrLn "Start"
        program <- readFile "test/test_program.kt"
        putStrLn $ addOuterClass $ removeComments program 0
        ast <- return $ parse parseProgram "" $ addOuterClass $ removeComments program 0
        pPrint ast
        runProgram ast
