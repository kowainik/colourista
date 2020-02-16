{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module introduces helpful pure codes to customise the terminal output view.
-}

module Colourista.Pure
    ( -- * Colour
      red
    , green
    , blue
    , yellow
    , black
    , white
    , magenta
    , cyan

      -- * Emphasis
    , bold
    , italic

      -- * Reset
    , reset
    ) where

import Data.ByteString (ByteString)
import Data.String (IsString (..))
import Data.Text (Text)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid), ConsoleIntensity (BoldIntensity),
                            ConsoleLayer (Foreground), SGR (..), setSGRCode)


-- | Code to apply 'Red' colouring for the terminal output.
red :: IsString str => str
red = fromString $ setSGRCode [SetColor Foreground Vivid Red]
{-# SPECIALIZE red :: String     #-}
{-# SPECIALIZE red :: Text       #-}
{-# SPECIALIZE red :: ByteString #-}

-- | Code to apply 'Green' colouring for the terminal output.
green :: IsString str => str
green = fromString $ setSGRCode [SetColor Foreground Vivid Green]
{-# SPECIALIZE green :: String     #-}
{-# SPECIALIZE green :: Text       #-}
{-# SPECIALIZE green :: ByteString #-}

-- | Code to apply 'Blue' colouring for the terminal output.
blue :: IsString str => str
blue = fromString $ setSGRCode [SetColor Foreground Vivid Blue]
{-# SPECIALIZE blue :: String     #-}
{-# SPECIALIZE blue :: Text       #-}
{-# SPECIALIZE blue :: ByteString #-}

-- | Code to apply 'Yellow' colouring for the terminal output.
yellow :: IsString str => str
yellow = fromString $ setSGRCode [SetColor Foreground Vivid Yellow]
{-# SPECIALIZE yellow :: String     #-}
{-# SPECIALIZE yellow :: Text       #-}
{-# SPECIALIZE yellow :: ByteString #-}

-- | Code to apply 'Black' colouring for the terminal output.
black :: IsString str => str
black = fromString $ setSGRCode [SetColor Foreground Vivid Black]
{-# SPECIALIZE black :: String     #-}
{-# SPECIALIZE black :: Text       #-}
{-# SPECIALIZE black :: ByteString #-}

-- | Code to apply 'White' colouring for the terminal output.
white :: IsString str => str
white = fromString $ setSGRCode [SetColor Foreground Vivid White]
{-# SPECIALIZE white :: String     #-}
{-# SPECIALIZE white :: Text       #-}
{-# SPECIALIZE white :: ByteString #-}

-- | Code to apply 'Magenta' colouring for the terminal output.
magenta :: IsString str => str
magenta = fromString $ setSGRCode [SetColor Foreground Vivid Magenta]
{-# SPECIALIZE magenta :: String     #-}
{-# SPECIALIZE magenta :: Text       #-}
{-# SPECIALIZE magenta :: ByteString #-}

-- | Code to apply 'Cyan' colouring for the terminal output.
cyan :: IsString str => str
cyan = fromString $ setSGRCode [SetColor Foreground Vivid Cyan]
{-# SPECIALIZE cyan :: String     #-}
{-# SPECIALIZE cyan :: Text       #-}
{-# SPECIALIZE cyan :: ByteString #-}

-- | Code to apply 'Bold' emphasis for the terminal output.
bold :: IsString str => str
bold = fromString $ setSGRCode [SetConsoleIntensity BoldIntensity]
{-# SPECIALIZE bold :: String     #-}
{-# SPECIALIZE bold :: Text       #-}
{-# SPECIALIZE bold :: ByteString #-}

-- | Code to apply 'Italic' emphasis for the terminal output.
italic :: IsString str => str
italic = fromString $ setSGRCode [SetItalicized True]
{-# SPECIALIZE italic :: String     #-}
{-# SPECIALIZE italic :: Text       #-}
{-# SPECIALIZE italic :: ByteString #-}

-- | Code to reset all previous code applied for the terminal output.
reset :: IsString str => str
reset = fromString $ setSGRCode [Reset]
{-# SPECIALIZE reset :: String     #-}
{-# SPECIALIZE reset :: Text       #-}
{-# SPECIALIZE reset :: ByteString #-}
