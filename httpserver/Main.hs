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
  putStrLn "Someone has connected."
  forkIO $ respond socketHandle 
  receiveMessages mySocket

respond :: Handle -> IO ()
respond socketHandle = do
  firstLine <- hGetLine socketHandle
  putStrLn firstLine
  case (getData $ words firstLine) of
    Just ("GET", address, _) -> respondToMsg socketHandle address
    _                        -> return ()

getData :: [String] -> Maybe (String, String, String)
getData ws = if length ws < 3 then Nothing else
  let fileName = foldl (\ s t -> s ++ (' ' : t)) (ws !! 1) (init $ drop 2 ws) in 
    Just (ws !! 0, fileName, last ws)

respondToMsg :: Handle -> String -> IO ()
respondToMsg socketHandle address' = do
  let address = tail address'
  exists <- doesFileExist address
  if not exists then hPutStrLn socketHandle "HTTP/1.1 404 Not Found\r" else do
    hPutStrLn socketHandle "HTTP/1.1 200 OK\r"
    hPutStrLn socketHandle "\r"
    putStrLn $ "Sending " ++ address
    openBinaryFile address ReadMode >>= writeToFile socketHandle
    hClose socketHandle

writeToFile :: Handle -> Handle -> IO ()
writeToFile socketHandle file = do
  hGetContents file >>= hPutStrLn socketHandle
  hClose file
