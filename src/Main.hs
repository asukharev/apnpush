module Main where

import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Tokens
import Payload
import APNS

data Flag
    = Message               -- -m
    | Tokens                -- -t
    | Cert                  -- -c
    | Key                   -- -k
    | Sandbox               -- -s
    | Help                  -- --help
    deriving (Eq, Ord, Enum, Show, Bounded)

flags :: [OptDescr Flag]
flags =
   [Option ['m'] []       (NoArg Message)
        "Message to send"
   ,Option ['t'] []       (NoArg Tokens)
        "File containing tokens of devices"
   ,Option ['s'] []       (NoArg Sandbox)
        "Use sandbox APNS"
   ,Option ['c'] []       (NoArg Cert)
        "Path to cert file"
   ,Option ['k'] []       (NoArg Key)
        "Path to key file"
   ,Option []    ["help"] (NoArg Help)
        "Print this help"
   ]

parse :: [String] -> IO ([Flag], [String])
parse argv =
  case getOpt Permute flags argv of
    (args, fs, []) ->
      do
        let files = if null fs then ["-"] else fs
        if Help `elem` args
          then
            do
              hPutStrLn stderr (usageInfo header flags)
              exitWith ExitSuccess
        else return (nub (concatMap set args), files)
    (_, _, errs) ->
      do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)
  where
    header = "Usage: apnspush [-tmcks] [tokens file, message, cert file, key file, sandbox]"
    set f = [f]

main :: IO ()
main =
  do
    (args, files) <- getArgs >>= parse
    let m = M.fromList $ zip (delete Sandbox args) files

    tokens <- getTokens $ m M.! Tokens
    let payload = Payload (m M.! Message) 0 SoundTypeDefault

    let apns_ssl_certificate_file = m M.! Cert
    let apns_ssl_private_key_file = m M.! Key

    putStrLn "Send payload"
    putStrLn (show payload)
    putStrLn "to registered devices"
    print tokens

    case (elem Sandbox args) of
      True ->
        pushMessTest apns_ssl_private_key_file apns_ssl_certificate_file (BL.pack (show payload)) (map (B.pack) tokens)
      False ->
        pushMessLive apns_ssl_private_key_file apns_ssl_certificate_file (BL.pack (show payload)) (map (B.pack) tokens)

    return ()
