module Main where

import System.Environment (getArgs)
import Network.Socket
import Data.Word (Word16)

main = do
  args <- getArgs
  let port = read (head args)::Word16
  mySocket <- socket AF_INET Stream defaultProtocol
  bindSocket mySocket $ SockAddrInet (PortNum port) 0
  putStrLn $ head args
