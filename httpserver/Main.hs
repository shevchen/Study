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
  isClosed <- hIsEOF socketHandle
  if isClosed then return () else do
    firstLine <- hGetLine socketHandle
    case (getData $ words firstLine) of
      Just ("GET", address, "HTTP/1.1") -> respondToMsg socketHandle address
      _                                 -> return ()
    respond socketHandle

getData :: [String] -> Maybe (String, String, String)
getData (x:(y:(z:[]))) = Just (x, y, z)
getData _              = Nothing

respondToMsg :: Handle -> String -> IO ()
respondToMsg socketHandle address = do
  exists <- doesFileExist address
  if not exists then hPutStrLn socketHandle "HTTP/1.1 404 Not Found" else do
    hPutStrLn socketHandle "HTTP/1.1 200 OK"
    hPutStrLn socketHandle ""
    openFile address ReadMode >>= writeToFile socketHandle

writeToFile :: Handle -> Handle -> IO ()
writeToFile socketHandle file = do
  isEnd <- hIsEOF file
  if isEnd then hClose file else do
    hGetLine file >>= hPutStrLn socketHandle
    writeToFile socketHandle file
