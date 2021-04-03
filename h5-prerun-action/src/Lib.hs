{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( prerun
    ) where

import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (join, when)
import Control.Exception (bracket_)

{-
-- Non-hardcore solution
prerun :: (IO a -> IO (IO b)) -> IO (IO a -> IO b)
prerun runner =
  do mutex <- newMVar ()
     jobCell <- newIORef (error "uninitialized cell")
     body <- runner $ join (readIORef jobCell)
     return \job ->
       withMVar mutex \() -> do
         writeIORef jobCell job
         body
-}

-- Implement reentrant lock
data LockOwner = NoOwner | Owner !ThreadId
newtype ReLock = ReLock (TVar LockOwner)

newReLock :: IO ReLock
newReLock = ReLock <$> newTVarIO NoOwner

withReLock :: ReLock -> IO a -> IO a
withReLock (ReLock ownerVar) = bracket_ acquire release
  where
    acquire = do
      myId <- myThreadId
      atomically do
        owner <- readTVar ownerVar
        case owner of
          NoOwner                         -> writeTVar ownerVar (Owner myId)
          Owner ownerId | myId == ownerId -> return ()
                        | otherwise       -> retry
    release = atomically $ writeTVar ownerVar NoOwner

loopWhile :: IO Bool -> IO ()
loopWhile mma =
  let loop = mma >>= \cond -> when cond loop
  in loop

prerun :: (IO a -> IO (IO b)) -> IO (IO a -> IO b)
prerun runner =
  do relock <- newReLock
     jobCell <- newIORef []
     body <- runner $ readIORef jobCell >>= head
     return \job ->
       withReLock relock do
         modifyIORef jobCell (job:)
         a <- body
         modifyIORef jobCell tail
         return a
