module ArgsParsing
(
    parseArgs,
    usage
) where

data ParseResult = Usage | Help | ParseError String

usageBase :: IO()
usageBase = putStrLn $ "usage: ft_turing [-h] jsonfile input"

usageVerbose :: IO()
usageVerbose = do
    usageBase
    putStrLn("\npositional arguments:")
    putStrLn("\tjsonfile\tjson description of the machine")
    putStrLn("\tinput\tinput of the machine")
    putStrLn("\noptional arguments:")
    putStrLn("\t-h, --help\tShow this help message and exit")

usageError :: String -> IO()
usageError error = do
    putStrLn error
    usageBase

usage :: ParseResult -> IO()
usage Usage = usageBase
usage Help = usageVerbose
usage (ParseError error) = usageError error

parseFlag :: String -> Either ParseResult (String, String)
parseFlag "-h" = Left Help
parseFlag "--help" = Left Help
parseFlag flag = Left $ ParseError $ "Unkown flag " ++ flag

parseArgs :: [String] -> Either ParseResult (String, String)
parseArgs [] = Left $ ParseError "Missing arguments"
parseArgs (fst:_) | head fst == '-' = parseFlag fst
parseArgs (filename:input:[]) = Right (filename, input)
parseArgs _ = Left $ ParseError "Wrong number of arguments"
