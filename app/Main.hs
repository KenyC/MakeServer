module Main where

import MakeServer
import System.Console.GetOpt
import System.Environment





options :: [OptDescr (ServerOpts -> ServerOpts)]
options =
    [ Option ['d'] ["directory"] (ReqArg setRootDir  "DIR")  "starts server with DIR as root dir"
    , Option ['p'] ["port"]      (ReqArg setBindPort "PORT") "starts server on port POST"
    ] 
    where setRootDir  dir  args = args {rootDir = dir  }  
          setBindPort port args = args {port    = read port }


main :: IO ()
main = do
    argv <- getArgs
    opts <- case getOpt Permute options argv of
       (o, _, []  ) -> return $ foldl (flip ($)) defaultServerOpts o
       (_, _, errs) -> ioError (userError (concat errs ++ usageInfo "" options))
    serverMain opts

