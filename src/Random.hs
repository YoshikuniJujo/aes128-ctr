{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Random (randomByteString) where

import Data.IORef
import System.IO.Unsafe
import Crypto.Random

import qualified Data.ByteString as BS

gen :: IORef SystemDRG
gen = unsafePerformIO $ newIORef =<< getSystemDRG

randomByteString :: Int -> IO BS.ByteString
randomByteString n = do
	g <- readIORef gen
	let	(bs, g') = randomBytesGenerate n g
	writeIORef gen g'
	return bs
