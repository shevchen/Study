module Main where

import System.Environment (getArgs)
import Network (listenOn, PortID(..), accept)
import Network.Socket (Socket)
import Control.Concurrent (forkIO)
import System.IO

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
    Just ("GET", address, "HTTP/1.1") -> respondToMsg socketHandle address >> respond socketHandle
    _                                 -> respond socketHandle

getData :: [String] -> Maybe (String, String, String)
getData []     = Nothing
getData (x:xs) = getRest x xs []

getRest :: String -> [String] -> String -> Maybe (String, String, String)
getRest method [] _            = Nothing
getRest method (x:[]) addr     = Just (method, addr, x)
getRest method (x:(y:ys)) addr = getRest method (y:ys) (addr ++ x)

respondToMsg :: Handle -> String -> IO ()
respondToMsg socketHandle address = do
  file <- openFile address ReadMode
  hClose file
