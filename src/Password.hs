{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Password where

import Data.ByteArray
import Data.Char
import Numeric
import Crypto.Hash

import qualified Data.ByteString as BS

nosalt :: BS.ByteString -> (BS.ByteString, BS.ByteString)
nosalt pswd = let
	key = convert $ hash @BS.ByteString @MD5 pswd
	iv = convert $ hash @BS.ByteString @MD5 $ key <> pswd in
	(key, iv)

hexdump :: BS.ByteString -> String
hexdump = map toUpper . ($ "") . foldr (.) id . map showHex . BS.unpack

salted :: BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString)
salted pswd slt = let
	key = convert . hash @BS.ByteString @MD5 $ pswd <> slt
	iv = convert . hash @BS.ByteString @MD5 $ key <> pswd <> slt in
	(key, iv)
