{-# LANGUAGE CPP #-}

{- |
Copyright: (c) 2020-2022 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains short aliases for most frequently used pure functions.
-}

module Colourista.Short
    ( b
    , i
    , u
    ) where

import Data.ByteString (ByteString)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup)
#endif
import Data.String (IsString)
import Data.Text (Text)

import Colourista.Pure (bold, formatWith, italic, underline)


-- | Short alias for 'bold'.
b :: (IsString str, Semigroup str) => str -> str
b = formatWith [bold]
{-# SPECIALIZE b :: String -> String         #-}
{-# SPECIALIZE b :: Text -> Text             #-}
{-# SPECIALIZE b :: ByteString -> ByteString #-}

-- | Short alias for 'italic'.
i :: (IsString str, Semigroup str) => str -> str
i = formatWith [italic]
{-# SPECIALIZE i :: String -> String         #-}
{-# SPECIALIZE i :: Text -> Text             #-}
{-# SPECIALIZE i :: ByteString -> ByteString #-}

-- | Short alias for 'underline'.
u :: (IsString str, Semigroup str) => str -> str
u = formatWith [underline]
{-# SPECIALIZE u :: String -> String         #-}
{-# SPECIALIZE u :: Text -> Text             #-}
{-# SPECIALIZE u :: ByteString -> ByteString #-}
