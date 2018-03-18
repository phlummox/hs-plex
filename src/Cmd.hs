
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
import System.IO                (hGetContents, stdout, hFlush)
import Control.Concurrent.Async (race)
import Control.Exception        (onException, evaluate)
import Control.Concurrent       (threadDelay)
import System.Posix.Types       (ProcessID)
import Control.DeepSeq          (force)

{-

foreign import ccall "cmd" cmd_c ::
  Ptr CChar -> Ptr CString -> Ptr CSize -> Ptr CString -> IO CInt
--int cmd(const char* comm, char* const args[], size_t* outSz, constCharP* out);

foreign import ccall "printf" printf_c ::
  CString -> IO ()


cmd_ :: String -> [String] -> IO String
cmd_ command args = do
  let args' = command : args
  -- convert args' to an array of cstrings that can be passed.
  args'' <- mapM newCString args'
  resStr <- withCString command $ \command' -> 
    withArrayLen0 (nullPtr :: CString) args'' $ \cnt args''' -> do
      -- we allocate memory for a CSize and a Ptr
      alloca $ \cSizePtr -> do
        alloca $ \wordPtr -> do
          let xxx = wordPtr :: Ptr WordPtr
          throwErrnoIf (/= 0) "cmd" $
            cmd_c command' args''' cSizePtr (castPtr wordPtr) 
          -- now get size ...
          size <- fromIntegral <$> peek cSizePtr
          resPtr <- wordPtrToPtr <$> peek wordPtr
          resStr <- peekCStringLen (resPtr, size)
          return resStr
  mapM_ free args''
  return resStr

-}

putStrLn' :: String -> IO ()
putStrLn' str = do
  !str' <- evaluate $ force (str ++ "\n")
  hFlush stdout
  putStr str'
  hFlush stdout

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
        executeFile command True args Nothing -- `onException`
          -- (putStrLn' "executeFile failed")
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

-- | same as command, but with a timeout
cmdTimeout :: FilePath -> [String] -> Int -> IO (Maybe String)
cmdTimeout command args microSecs = do
  (childPid, readerTask) <- readCommand command args
  let
      timerTask :: Int -> IO ()
      timerTask n = do
        threadDelay n
        childGroupID <- getProcessGroupIDOf childPid
        signalProcessGroup killProcess childGroupID
  res <- race  (timerTask microSecs) readerTask
  return $ rightToMaybe res

