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

      -- * Background
    , redBg
    , greenBg
    , blueBg
    , yellowBg
    , blackBg
    , whiteBg
    , magentaBg
    , cyanBg

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
                            ConsoleLayer (Background, Foreground), SGR (..), setSGRCode)


----------------------------------------------------------------------------
-- Colours
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- Background
----------------------------------------------------------------------------

-- | Code to apply 'Red' background colouring for the terminal output.
redBg :: IsString str => str
redBg = fromString $ setSGRCode [SetColor Background Vivid Red]
{-# SPECIALIZE redBg :: String     #-}
{-# SPECIALIZE redBg :: Text       #-}
{-# SPECIALIZE redBg :: ByteString #-}

-- | Code to apply 'Green' background colouring for the terminal output.
greenBg :: IsString str => str
greenBg = fromString $ setSGRCode [SetColor Background Vivid Green]
{-# SPECIALIZE greenBg :: String     #-}
{-# SPECIALIZE greenBg :: Text       #-}
{-# SPECIALIZE greenBg :: ByteString #-}

-- | Code to apply 'Blue' background colouring for the terminal output.
blueBg :: IsString str => str
blueBg = fromString $ setSGRCode [SetColor Background Vivid Blue]
{-# SPECIALIZE blueBg :: String     #-}
{-# SPECIALIZE blueBg :: Text       #-}
{-# SPECIALIZE blueBg :: ByteString #-}

-- | Code to apply 'Yellow' background colouring for the terminal output.
yellowBg :: IsString str => str
yellowBg = fromString $ setSGRCode [SetColor Background Vivid Yellow]
{-# SPECIALIZE yellowBg :: String     #-}
{-# SPECIALIZE yellowBg :: Text       #-}
{-# SPECIALIZE yellowBg :: ByteString #-}

-- | Code to apply 'Black' background colouring for the terminal output.
blackBg :: IsString str => str
blackBg = fromString $ setSGRCode [SetColor Background Vivid Black]
{-# SPECIALIZE blackBg :: String     #-}
{-# SPECIALIZE blackBg :: Text       #-}
{-# SPECIALIZE blackBg :: ByteString #-}

-- | Code to apply 'White' background colouring for the terminal output.
whiteBg :: IsString str => str
whiteBg = fromString $ setSGRCode [SetColor Background Vivid White]
{-# SPECIALIZE whiteBg :: String     #-}
{-# SPECIALIZE whiteBg :: Text       #-}
{-# SPECIALIZE whiteBg :: ByteString #-}

-- | Code to apply 'Magenta' background colouring for the terminal output.
magentaBg :: IsString str => str
magentaBg = fromString $ setSGRCode [SetColor Background Vivid Magenta]
{-# SPECIALIZE magentaBg :: String     #-}
{-# SPECIALIZE magentaBg :: Text       #-}
{-# SPECIALIZE magentaBg :: ByteString #-}

-- | Code to apply 'Cyan' background colouring for the terminal output.
cyanBg :: IsString str => str
cyanBg = fromString $ setSGRCode [SetColor Background Vivid Cyan]
{-# SPECIALIZE cyanBg :: String     #-}
{-# SPECIALIZE cyanBg :: Text       #-}
{-# SPECIALIZE cyanBg :: ByteString #-}

----------------------------------------------------------------------------
-- Emphasis
----------------------------------------------------------------------------

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
