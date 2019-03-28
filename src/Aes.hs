{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Aes (encryptCtr, decryptCtr, IV, AES128, CryptoError) where

import Crypto.Random.Types
import Crypto.Cipher.Types
import Crypto.Cipher.AES
import Crypto.Error

import qualified Data.ByteString as BS

encryptCtr :: BS.ByteString -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
encryptCtr k p = do
	bs :: BS.ByteString <- getRandomBytes $ blockSize @AES128 undefined
	e <- either (error . show) return $ xxcryptCtr k bs p
	return (bs, e)

decryptCtr, xxcryptCtr :: BS.ByteString -> BS.ByteString -> BS.ByteString ->
	Either CryptoError BS.ByteString
decryptCtr = xxcryptCtr
xxcryptCtr k i pe = case cipherInit @AES128 k of
	CryptoFailed e -> Left e
	CryptoPassed a -> case makeIV i of
		Just iv -> Right $ ctrCombine a iv pe
		Nothing -> Left CryptoError_IvSizeInvalid
