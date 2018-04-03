module System.Plex.ByteString where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy (ByteString)
import System.Plex.Internal hiding (cmd, cmdTimeout, readCommand)
import System.Posix.Types (ProcessID)

-- | @readCommand command args@: given a /command/ to run in a subprocess,
--  and /args/ to pass to it --
-- return the childPid of the subprocess, and a function for
-- reading its output.
--
-- if it doesn't start with a forward-slash, the /command/ is searched for
-- in the current path.
readCommand :: FilePath -> [String] -> IO (ProcessID, IO ByteString)
readCommand command args = readCommand_ L.hGetContents command args

-- | @cmd command args@: execute /command/ with /args/; read combined stdout and stderr;
-- return it as a string
-- If there is an error executing the command, it'll get caught
-- and a Haskell error message will get printed.
cmd :: FilePath -> [String] -> IO ByteString
cmd command args = do
  (childPid, readerTask) <- readCommand command args
  readerTask

-- | same as command, but with a timeout.
-- Hard to get it do return nicely with timeout < about 10^5 microsecs 
cmdTimeout :: FilePath -> [String] -> Int -> IO (Maybe ByteString)
cmdTimeout command args microSecs =
  cmdTimeout_ L.hGetContents command args microSecs
