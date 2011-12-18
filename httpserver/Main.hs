module Main where

import System.Environment (getArgs)
import Network (listenOn, PortID(..), accept)
import Network.Socket (Socket)
import Control.Concurrent (forkIO)
import System.IO
import System.Directory (doesFileExist)

main :: IO ()
main = do
  args <- getArgs
  let port = read (head args)::Int
  mySocket <- listenOn $ PortNumber $ fromIntegral port
  receiveMessages mySocket

receiveMessages :: Socket -> IO ()
receiveMessages mySocket = do
  (socketHandle, _, _) <- accept mySocket
  forkIO $ respond socketHandle 
  receiveMessages mySocket

respond :: Handle -> IO ()
respond socketHandle = do
  firstLine <- hGetLine socketHandle
  case (getData $ words firstLine) of
    Just ("GET", address, "HTTP/1.1") -> respondToMsg socketHandle address
    _                                 -> return ()
  respond socketHandle

getData :: [String] -> Maybe (String, String, String)
getData []     = Nothing
getData (x:xs) = getRest x xs []

getRest :: String -> [String] -> String -> Maybe (String, String, String)
getRest method [] _            = Nothing
getRest method (x:[]) addr     = Just (method, addr, x)
getRest method (x:(y:ys)) addr = getRest method (y:ys) (addr ++ x)

respondToMsg :: Handle -> String -> IO ()
respondToMsg socketHandle address = do
  exists <- doesFileExist address
  if not exists then hPutStrLn socketHandle "HTTP/1.1 404 Not Found" else 
    hPutStrLn socketHandle "HTTP/1.1 200 OK" >>
    hPutStrLn socketHandle "" >>
    openFile address ReadMode >>=
    writeToFile socketHandle

writeToFile :: Handle -> Handle -> IO ()
writeToFile socketHandle file = do
  isEnd <- hIsEOF file
  if isEnd then hClose file else
    hGetLine file >>=
    hPutStrLn socketHandle >>
    writeToFile socketHandle file
