import System.IO
import Parser


repl :: IO ()
repl = do
        putStr "> "
        hFlush stdout
        s <- getLine

        case s of
            "q" -> return () 
            "" -> repl
            otherwise -> do
                either putStrLn (putStrLn . show) (evalRPNExpression s)
                repl

main :: IO ()
main = do
            putStrLn "  \n\
                     \  RPN expression evaluator \n\
                     \  \n\
                     \  Supported functions: + - * / sin cos exp sqrt \n\
                     \  Constants: pi e phi \n\
                     \  \n\
                     \  To exit, type \"q\" \
                     \  \n "
            repl
