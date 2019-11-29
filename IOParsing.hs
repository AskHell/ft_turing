module IOParsing
(
    parseArgs,
    usage
) where

data IOResult a = Usage | Help | IOError a

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

usage :: IOResult String -> IO()
usage Usage = usageBase
usage Help = usageVerbose
usage (IOError error) = usageError error

parseFlag :: String -> Either (IOResult String) (String, String)
parseFlag "-h" = Left Help
parseFlag "--help" = Left Help
parseFlag flag = Left $ IOError $ "Unkown flag " ++ flag

parseArgs :: [String] -> Either (IOResult String) (String, String)
parseArgs [] = Left $ IOError "Missing arguments"
parseArgs (fst:_) | head fst == '-' = parseFlag fst
parseArgs (filename:input:[]) = Right (filename, input)
parseArgs _ = Left $ IOError "Wrong number of arguments"
