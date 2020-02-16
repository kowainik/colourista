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

import System.Console.ANSI (Color (..), ColorIntensity (Vivid), ConsoleIntensity (BoldIntensity),
                            ConsoleLayer (Foreground), SGR (..), setSGRCode)


-- | Code to apply 'Red' colouring for the terminal output.
red :: String
red = setSGRCode [SetColor Foreground Vivid Red]

-- | Code to apply 'Green' colouring for the terminal output.
green :: String
green = setSGRCode [SetColor Foreground Vivid Green]

-- | Code to apply 'Blue' colouring for the terminal output.
blue :: String
blue = setSGRCode [SetColor Foreground Vivid Blue]

-- | Code to apply 'Yellow' colouring for the terminal output.
yellow :: String
yellow = setSGRCode [SetColor Foreground Vivid Yellow]

-- | Code to apply 'Black' colouring for the terminal output.
black :: String
black = setSGRCode [SetColor Foreground Vivid Black]

-- | Code to apply 'White' colouring for the terminal output.
white :: String
white = setSGRCode [SetColor Foreground Vivid White]

-- | Code to apply 'Magenta' colouring for the terminal output.
magenta :: String
magenta = setSGRCode [SetColor Foreground Vivid Magenta]

-- | Code to apply 'Cyan' colouring for the terminal output.
cyan :: String
cyan = setSGRCode [SetColor Foreground Vivid Cyan]

-- | Code to apply 'Bold' emphasis for the terminal output.
bold :: String
bold = setSGRCode [SetConsoleIntensity BoldIntensity]

-- | Code to apply 'Italic' emphasis for the terminal output.
italic :: String
italic = setSGRCode [SetItalicized True]

-- | Code to reset all previous code applied for the terminal output.
reset :: String
reset = setSGRCode [Reset]
