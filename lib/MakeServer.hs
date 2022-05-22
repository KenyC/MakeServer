{-# LANGUAGE OverloadedStrings #-}
module MakeServer where

import Blaze.ByteString.Builder (copyByteString, Builder)
import Control.Monad
import Control.Monad.Writer.Strict
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Text            as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.Mime
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types 
import System.Directory
import System.FilePath
import System.Exit
import System.Process
import Text.Printf

data ServerOpts = ServerOpts {
      rootDir :: FilePath
    , port    :: Int
} deriving (Show)
 
defaultServerOpts = ServerOpts {
      rootDir = "."
    , port = 3000
}

serverMain :: ServerOpts -> IO () 
serverMain ServerOpts{..} = do
    hasMakefile <- doesPathExist $ rootDir </> "Makefile"
    putStrLn $ printf "Server started at http://0.0.0.0:%d/"  port
    when (not hasMakefile) $ do
        putStrLn "WARNING: No Makefile found ; server will not compile anything"
    run port (app rootDir hasMakefile)
 
-- | Respond to request. 
--   If path requested points to a directory, a listing of that directory is returned
--   If path requested points to a file and there is a make file in server's root directory, `make` is called to recompile the file (if need be) and the file is returned
app :: FilePath -> Bool -> Request -> (Response -> IO b) -> IO b
app mainDir hasMakefile req respond = withRequest req $ do

    -- let (path, mimetype) = makePath (pathInfo req) "" mainDir

    let relativePath = joinPath $ map T.unpack $ pathInfo req
    let path         = mainDir </> relativePath

    isPath <- doesPathExist      path
    isDir  <- doesDirectoryExist path

    print (isPath, isDir)

    if | isDir -> do
            putStrLn "displaying dir"
            dirList <- makeDirectoryListing mainDir relativePath
            respond $ responseBuilder
                status404 [("Content-Type", "text/html")] $ 
                    dirList
       | isPath -> do
            let mimetype = defaultMimeLookup $ last $ pathInfo req

            if hasMakefile
            then do
                putStr   $ "Recompiling ..."
                (stdout, stderr, success) <- recompile mainDir relativePath
                if success 
                then do
                    putStrLn $ "done."
                    putStrLn stdout
                    putStrLn stderr
                    respond $ responseFile
                        status200
                        [("Content-Type", mimetype)]
                        path
                        Nothing
                else do
                    putStrLn "error !"
                    putStrLn stdout
                    putStrLn stderr
                    respond $ responseBuilder
                        status404 [("Content-Type", "text/plain")] $ 
                            mconcat $ map copyByteString
                                [ BU.fromString stdout
                                , BU.fromString stderr ]
            else do
                putStrLn $ "No compilation since no Makefile."
                respond $ responseFile
                    status200
                    [("Content-Type", mimetype)]
                    path
                    Nothing
       | otherwise -> do
            putStrLn "Not a valid path or no file her"
            respond $ responseBuilder
                status404 [("Content-Type", "text/plain")] $ 
                    mconcat $ map copyByteString 
                        [ "Not a valid file or no file at "
                        , encodeUtf8 . T.pack $ path]



-- | Recompiles all files
recompile :: FilePath -> FilePath -> IO (String, String, Bool)
recompile mainDir fileRelPath = do
    (code, stdout, stderr) <- readProcessWithExitCode 
                               "make"
                               ["-C", mainDir, fileRelPath]
                               ""

    let success = case code of
         ExitSuccess   -> True
         ExitFailure _ -> False
 
    return (stdout, stderr, success)


-- | Produces a Builder for the html content of directory listing
makeDirectoryListing :: 
    FilePath    -- ^ path to server's root directory 
 -> FilePath    -- ^ path from root directory to directory to be listed
 -> IO Builder
makeDirectoryListing mainDir relativePath = do
    let makeBs = encodeUtf8 . T.pack
    let putBs  = tell . copyByteString

    let relativePathBs = makeBs relativePath
    let absolutePath = mainDir </> relativePath

    entries <- listDirectory absolutePath

    execWriterT $ do

        let title  = "Directory listing for " <> relativePathBs
        putBs $ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"'\"http://www.w3.org/TR/html4/strict.dtd\">"
        putBs $ "<html>\n<head>"
        putBs $ "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf8\">"
        putBs $ "<title>" <> title <> "</title>\n</head>"
        putBs $ "<body>\n<h1>" <> title <> "</h1>" 
        putBs $ "<hr>\n<ul>"

        forM_ entries $ \entry -> do
            let fullPath = absolutePath </> entry
            let urlPath  = relativePath </> entry

            isDir  <- liftIO $ doesDirectoryExist fullPath
            isLink <- liftIO $ pathIsSymbolicLink fullPath
            let displayPath 
                    | isDir     = entry ++ "/"
                    | isLink    = entry ++ "@"
                    | otherwise = entry

            putBs $ makeBs $ printf "<li><a href=\"/%s\">%s</a></li>" urlPath displayPath

        putBs $ "</ul>\n<hr>\n</body>\n</html>\n"


-- | Given request, encircles log with "<==" and "==>", displaying relevant info about request
withRequest :: 
    Request  
 -> IO a    -- ^ continuation
 -> IO a
withRequest req action = do
    let method = either show show $ parseMethod $ requestMethod req
    putStrLn $ printf "<== %s %s" method (show $ rawPathInfo req) 
    result <- action
    putStrLn "==>"
    return result
