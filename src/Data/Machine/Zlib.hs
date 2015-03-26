{-# LANGUAGE NoImplicitPrelude #-}
module Data.Machine.Zlib where

import BasicPrelude
import Control.Exception (throw)
import Data.Machine
import Data.Streaming.Zlib

-- | Even though this doesn't actually do any IO, we stay in the IO monad
-- like Data.Streaming.Zlib does, to avoid any unsafe "black magic".
gunzipper :: (Functor m, MonadIO m) => ProcessT m ByteString ByteString
gunzipper = construct $ do
  inflate <- liftIO . initInflate $ WindowBits 31
  let go :: (Functor m, MonadIO m) => PlanT (Is ByteString) ByteString m ()
      go = do
        bs <- await <|> finish
        popper <- liftIO <$> liftIO (feedInflate inflate bs)
        exhaust (handleRes <$> popper) <|> go
      handleRes PRDone = Nothing
      handleRes (PRNext bs) = Just bs
      handleRes (PRError e) = throw e
      finish :: MonadIO m => PlanT (Is ByteString) ByteString m ByteString
      finish = do
        r <- liftIO $ finishInflate inflate
        yield r
        stop
  go
