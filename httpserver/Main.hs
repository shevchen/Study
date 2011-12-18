module Main where

import System.Environment (getArgs)
import Network.Socket
import Data.Word (Word16)
import Control.Concurrent (forkIO)

main = do
  args <- getArgs
  let port = read (head args)::Word16
  mySocket <- socket AF_INET Stream defaultProtocol
  bindSocket mySocket $ SockAddrInet (PortNum port) 0
  listen mySocket 10
  receiveMessages mySocket
  putStrLn $ head args

receiveMessages mySocket = do
  forkIO $ recv mySocket 1024 >>= putStrLn
