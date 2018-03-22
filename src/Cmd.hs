
{-# LANGUAGE BangPatterns #-}

module Cmd (
      readCommand
    , cmd
    , cmdTimeout
  )
  where

-- import Foreign hiding (void)
-- import Foreign.C.Types
-- import Foreign.C.String
-- import Foreign.C.Error
-- import Foreign.Marshal.Array
-- import Foreign.Marshal.Alloc
import Control.Monad
import System.Posix.Process     (createSession, executeFile, forkProcess,
                                getProcessGroupIDOf)
import System.Posix.IO          (closeFd, stdError, stdOutput, dupTo,
                                 createPipe, fdToHandle)
import System.Posix.Signals     (signalProcessGroup, killProcess)
import System.IO                (hGetContents, stdout, hFlush, hPutStrLn,
                                  stderr)
import Control.Concurrent.Async (race, waitEitherCatch, withAsync)
import Control.Exception        (onException, evaluate, try, SomeException)
import Control.Concurrent       (threadDelay)
import System.Posix.Types       (ProcessID)
import Control.DeepSeq          (force)
import Data.Either              (either)

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try


putStrLn' :: String -> IO ()
putStrLn' str = do
  !str' <- evaluate $ force (str ++ "\n")
  hFlush stdout
  putStr str'
  hFlush stdout

-- wrapped executeFile that reports error
executeFile'
  :: FilePath -> Bool -> [String] -> Maybe [(String, String)] -> IO ()
executeFile' cmd shell args env =
  tryAny (executeFile cmd shell args env) >>=
    either (\ex -> hPutStrLn stderr $ "error executing " ++ cmd ++ ", " ++
                                   show ex)
           (error "shouldn't be here")


-- | @readCommand command args@: given a /command/ to run in a subprocess,
--  and /args/ to pass to it --
-- return the childPid of the subprocess, and a function for
-- reading its output.
--
-- if it doesn't start with a forward-slash, the /command/ is searched for
-- in the current path.
readCommand
  :: FilePath -> [String] -> IO (ProcessID, IO String)
readCommand command args = do
  (readPipeEnd, writePipeEnd) <- createPipe
  let childTask :: IO ()
      childTask = do
        void createSession 
        void $ dupTo writePipeEnd stdOutput
        void $ dupTo writePipeEnd stdError
        -- child doesn't need these, so close
        closeFd readPipeEnd
        closeFd writePipeEnd
        executeFile' command True args Nothing 
  !childPid <- forkProcess childTask
  -- in parent
  closeFd writePipeEnd
  let readerTask :: IO String
      readerTask = do
        !readPipeHdl <- fdToHandle readPipeEnd
        result <- hGetContents readPipeHdl `onException` 
                      closeFd readPipeEnd
        !_ <- evaluate $ force result
        return result
  return (childPid, readerTask)

-- | @cmd command args@: execute /command/ with /args/; read combined stdout and stderr;
-- return it as a string
-- If there is an error executing the command, it'll get caught
-- and a Haskell error message will get printed.
cmd :: FilePath -> [String] -> IO String
cmd command args = do
  (childPid, readerTask) <- readCommand command args
  readerTask

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

race'
  :: IO a
     -> IO b
     -> IO (Either (Either SomeException a) (Either SomeException b))
race' left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitEitherCatch a b

type SEx = SomeException  

-- | same as command, but with a timeout.
-- Hard to get it do return nicely with timeout < about 10^5 microsecs 
cmdTimeout :: FilePath -> [String] -> Int -> IO (Maybe String)
cmdTimeout command args microSecs 
  | microSecs <= 0  = return Nothing
  | otherwise       = 
      do
        (childPid, readerTask) <- readCommand command args
        let
            timerTask :: Int -> IO ()
            timerTask n = do
              threadDelay n
              !childGroupID <- getProcessGroupIDOf childPid
              !_ <- signalProcessGroup killProcess childGroupID
              return ()

            timerTask' n =
              timerTask n `onException` return ()

        rightToMaybe . onlyA <$> race' (timerTask' microSecs) readerTask
        -- let zzz = resX :: Maybe String

        -- res <- tryAny (race  (timerTask' microSecs) readerTask) >>= \exOrRes ->
        --           case exOrRes of
        --             Left ex -> return $ Left ()
        --             Right res -> return res
        -- return $ rightToMaybe res
      where
        onlyA :: Either (Either SEx ()) (Either SEx a) -> Either () a
        onlyA = either (const $ Left ()) (either (const $ Left ())  Right)


