module Input where

import System.IO (stdin, hReady, hSetEcho, hSetBuffering, BufferMode(..))
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad (liftM2)

data Key = ArrowUp | ArrowLeft | ArrowRight | Esc | EndOfText | Other String deriving (Eq, Show)

stringToKey :: String -> Key
stringToKey s = case s of
    "\ESC"   -> Esc
    "\ESC[A" -> ArrowUp
    "\ESC[C" -> ArrowRight
    "\ESC[D" -> ArrowLeft
    "\EOT"   -> EndOfText
    _        -> Other s

getKey :: IO Key
getKey = getKey' ""
  where
    getKey' :: String -> IO Key
    getKey' cs = do 
        c    <- getChar
        more <- hReady stdin
        let chars = c : cs
        if more then getKey' chars else return $ stringToKey $ reverse chars

getKeys :: IO [Key]
getKeys = unsafeInterleaveIO $ do
    key <- getKey
    if key == EndOfText then return [] else do
        next <- getKeys
        return (key : next)

setupStdin :: IO ()
setupStdin = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
