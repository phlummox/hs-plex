{-# LANGUAGE BangPatterns #-}

{- |

Generic versions of 'cmd', 'cmdTimeout' etc

-}

module System.Plex.Internal (
    executeFile'
  , readCommand
  , readCommand_
  , cmd
  , cmd_
  , cmdTimeout
  , cmdTimeout_
) where

import Control.Concurrent.Async   (waitEitherCatch, withAsync)
import Control.DeepSeq            (NFData, rnf )
import Control.Exception          (onException, evaluate, try, SomeException)
import Control.Concurrent         (threadDelay)
import Control.Monad
import Data.Either                (either)
import System.IO                  (hGetContents, hPutStrLn, stderr, Handle)
import System.Posix.Process       (createSession, executeFile, forkProcess,
                                  getProcessGroupIDOf)
import System.Posix.IO            (closeFd, stdError, stdOutput, dupTo,
                                   createPipe, fdToHandle)
import System.Posix.Signals       (signalProcessGroup, killProcess)
import System.Posix.Types         (ProcessID)

-- | catch all exceptions
tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

-- | wrapped executeFile that reports error
executeFile' ::
     FilePath -> Bool -> [String] -> Maybe [(String, String)] -> IO ()
executeFile' cmd shell args env =
  tryAny (executeFile cmd shell args env) >>=
  either
    (\ex -> hPutStrLn stderr $ "error executing " ++ cmd ++ ", " ++ show ex)
    (error "shouldn't be here")

-- | @readCommand command args@: given a /command/ to run in a subprocess,
--  and /args/ to pass to it --
-- return the childPid of the subprocess, and a function for
-- reading its output.
--
-- if it doesn't start with a forward-slash, the /command/ is searched for
-- in the current path.
readCommand :: FilePath -> [String] -> IO (ProcessID, IO String)
readCommand command args = readCommand_ hGetContents command args

-- | More general version of 'readCommand_'. First arg is
-- a function to read from the combined stdout/stderr 
-- handle.
readCommand_
  :: NFData a =>
     (Handle -> IO a) -> FilePath -> [String] -> IO (ProcessID, IO a)
readCommand_ hGetContents command args = do
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
      --readerTask :: IO String
  let readerTask = do
        !readPipeHdl <- fdToHandle readPipeEnd
        result <- hGetContents readPipeHdl `onException` closeFd readPipeEnd
        evaluate $ rnf result
        return result
  return (childPid, readerTask)

-- | @cmd command args@: execute /command/ with /args/; read combined stdout and stderr;
-- return it as a string
-- If there is an error executing the command, it'll get caught
-- and a Haskell error message will get printed.
cmd :: FilePath -> [String] -> IO String
cmd command args = cmd_ hGetContents command args

-- | more general version of 'cmd'.
cmd_ :: NFData b => (Handle -> IO b) -> FilePath -> [String] -> IO b
cmd_ hGetContents command args = do
  (_childPid, readerTask) <- readCommand_ hGetContents command args
  readerTask

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

race' ::
     IO a
  -> IO b
  -> IO (Either (Either SomeException a) (Either SomeException b))
race' left right =
  withAsync left $ \a -> withAsync right $ \b -> waitEitherCatch a b

type SEx = SomeException

-- | same as command, but with a timeout.
-- Hard to get it do return nicely with timeout < about 10^5 microsecs 
cmdTimeout :: FilePath -> [String] -> Int -> IO (Maybe String)
cmdTimeout command args microSecs =
  cmdTimeout_ hGetContents command args microSecs

-- | more general version of 'cmdTimeout'. 
cmdTimeout_ ::
     NFData a => (Handle -> IO a) -> FilePath -> [String] -> Int -> IO (Maybe a)
cmdTimeout_ hGetContents command args microSecs
  | microSecs <= 0 = return Nothing
  | otherwise = do
    (childPid, readerTask) <- readCommand_ hGetContents command args
    let timerTask :: Int -> IO ()
        timerTask n = do
          threadDelay n
          !childGroupID <- getProcessGroupIDOf childPid
          !_ <- signalProcessGroup killProcess childGroupID
          return ()
        timerTask' n = timerTask n `onException` return ()
    rightToMaybe . onlyA <$> race' (timerTask' microSecs) readerTask
  where
    onlyA :: Either (Either SEx ()) (Either SEx a) -> Either () a
    onlyA = either (const $ Left ()) (either (const $ Left ()) Right)
