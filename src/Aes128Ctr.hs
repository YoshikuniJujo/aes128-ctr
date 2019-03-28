{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Aes128Ctr (Password(..), encrypt, decrypt) where

import Data.String

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Aes
import Password
import Random

newtype Password = Password BS.ByteString deriving Show

instance IsString Password where
	fromString = Password . BSC.pack

encrypt :: Password -> BS.ByteString -> IO BS.ByteString
encrypt (Password pswd) pln = do
	slt <- randomByteString 8
	return . either (error . show) (("Salted__" <> slt) <>)
		$ uncurry decryptCtr (salted pswd slt) pln

decrypt :: Password -> BS.ByteString -> Either String BS.ByteString
decrypt (Password pswd) c = do
	(slt, crpt) <- spanSalt c
	let	(key, iv) = salted pswd slt
	either (Left . show) Right $ decryptCtr key iv crpt

spanSalt :: BS.ByteString -> Either String (BS.ByteString, BS.ByteString)
spanSalt c = case BS.splitAt 8 c of
	("Salted__", dt) -> Right $ BS.splitAt 8 dt
	_ -> Left "Not salted cryptogram"
