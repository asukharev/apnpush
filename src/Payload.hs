module Payload where

data SoundType =
    SoundTypeDefault
  | SoundTypeNone
  deriving (Eq, Ord, Enum, Show, Bounded)

data Payload = Payload {
  message :: String,
  badge   :: Integer,
  sound   :: SoundType
}

instance Show Payload where
  show p =
    "{" ++ show "aps"
    ++ ":{"
    ++ show "alert" ++ ":" ++ show (message p) ++ ","
    ++ show "badge" ++ ":" ++ show (badge p) ++ ","
    ++ show "sound" ++ ":"
    ++ case sound p of
      SoundTypeDefault -> show "default"
      SoundTypeNone    -> show "none"
    ++ "}}"
