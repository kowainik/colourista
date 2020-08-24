{-# LANGUAGE CPP #-}

{- |
Copyright: (c) 2020 Kowainik
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

import Colourista.Mode (HasColourMode)
import Colourista.Pure (bold, formatWith, italic, underline)


-- | Short alias for 'bold'.
b :: (HasColourMode, IsString str, Semigroup str) => str -> str
b = formatWith [bold]
{-# SPECIALIZE b :: HasColourMode => String -> String         #-}
{-# SPECIALIZE b :: HasColourMode => Text -> Text             #-}
{-# SPECIALIZE b :: HasColourMode => ByteString -> ByteString #-}

-- | Short alias for 'italic'.
i :: (HasColourMode, IsString str, Semigroup str) => str -> str
i = formatWith [italic]
{-# SPECIALIZE i :: HasColourMode => String -> String         #-}
{-# SPECIALIZE i :: HasColourMode => Text -> Text             #-}
{-# SPECIALIZE i :: HasColourMode => ByteString -> ByteString #-}

-- | Short alias for 'underline'.
u :: (HasColourMode, IsString str, Semigroup str) => str -> str
u = formatWith [underline]
{-# SPECIALIZE u :: HasColourMode => String -> String         #-}
{-# SPECIALIZE u :: HasColourMode => Text -> Text             #-}
{-# SPECIALIZE u :: HasColourMode => ByteString -> ByteString #-}
