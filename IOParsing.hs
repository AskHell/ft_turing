module IOParsing
(
    parseArgs,
    usage
) where

import qualified Data.ByteString.Lazy as B (readFile, ByteString)

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

parseFlag :: String -> Either (IOResult String) (B.ByteString, String)
parseFlag "-h" = Left Help
parseFlag "--help" = Left Help
parseFlag flag = Left $ IOError $ "Unkown flag " ++ flag

parseArgs :: [String] -> IO (Either (IOResult String) (B.ByteString, String))
parseArgs [] = return $ Left $ IOError "Missing arguments"
parseArgs (fst:_) | head fst == '-' = return $ parseFlag fst
parseArgs (filename:input:[]) = do
    description <- B.readFile filename
    return $ Right (description, input)
parseArgs _ = return $ Left $ IOError "Wrong number of arguments"
