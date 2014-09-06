{-# LANGUAGE OverloadedStrings #-}

-- Module:      Data.Text.Lazy.Builder.Int.Digits
-- Copyright:   (c) 2013 Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- This module exists because the C preprocessor does things that we
-- shall not speak of when confronted with Haskell multiline strings.

module Data.Text.Lazy.Builder.Int.Digits (digits) where

import Data.ByteString.Char8 (ByteString)

digits :: ByteString
digits = "0001020304050607080910111213141516171819\
         \2021222324252627282930313233343536373839\
         \4041424344454647484950515253545556575859\
         \6061626364656667686970717273747576777879\
         \8081828384858687888990919293949596979899"
