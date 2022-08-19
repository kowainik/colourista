{- |
Copyright: (c) 2020-2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module introduces helpful pure codes to customise the terminal output view.
-}

module Colourista.Pure
    ( formatWith
      -- * Colour
    , red
    , green
    , blue
    , yellow
    , black
    , white
    , magenta
    , cyan
    , rgb

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
    , underline
    , doubleUnderline
    , noUnderline
    , indent

      -- * Reset
    , reset
    ) where

import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Int (Int8)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Word (Word8)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid), ConsoleIntensity (BoldIntensity),
                            ConsoleLayer (Background, Foreground), SGR (..), Underlining (..),
                            setSGRCode)

import Data.Colour.SRGB (sRGB24)
import Colourista.Mode (HasColourMode, withColourMode)


{- | General purpose function to format strings with multiple
options. If this function takes empty list as an argument, no
formatting is applied.

Some typical usages include but not limited to:

1. Green text: @'formatWith' ['green'] myString@
2. Bold red text: @'formatWith' ['bold', 'red'] myString@
3. Blue text on white background: @'formatWith' ['blue', 'whiteBg'] myString@
4. Italicized yellow on cyan background: @'formatWith' ['italic', 'yellow', 'cyanBg'] myString@

![Colored examples](https://user-images.githubusercontent.com/4276606/74608609-8acced80-50da-11ea-9a32-e64eba6935c1.png)

__⚠ Caution:__ Double underlining 'doubleUnderline' is not widely supported.
It is also not natively supported on Windows 10.

-}
formatWith
    :: (IsString str, Semigroup str)
    => [str]
    -> str
    -> str
formatWith formatting str = case formatting of
    []   -> str
    x:xs -> sconcat (x :| xs) <> str <> reset
{-# SPECIALIZE formatWith :: [String]     -> String     -> String     #-}
{-# SPECIALIZE formatWith :: [Text]       -> Text       -> Text       #-}
{-# SPECIALIZE formatWith :: [ByteString] -> ByteString -> ByteString #-}

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

-- | Code to apply any arbitrary hex color for the terminal output.
--
-- >>> rgb 0xff 0x00 0x00 "I am red"
-- >>> rgb 0xff 0xff 0xff "I am white"
rgb :: (HasColourMode, IsString str) => Word8 -> Word8 -> Word8 -> str
rgb r g b = withColourMode $ fromString $ setSGRCode [SetRGBColor Foreground (sRGB24 r g b)]
    
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

-- | Code to apply __bold__ emphasis for the terminal output.
bold :: IsString str => str
bold = fromString $ setSGRCode [SetConsoleIntensity BoldIntensity]
{-# SPECIALIZE bold :: String     #-}
{-# SPECIALIZE bold :: Text       #-}
{-# SPECIALIZE bold :: ByteString #-}

-- | Code to apply /italic/ emphasis for the terminal output.
italic :: IsString str => str
italic = fromString $ setSGRCode [SetItalicized True]
{-# SPECIALIZE italic :: String     #-}
{-# SPECIALIZE italic :: Text       #-}
{-# SPECIALIZE italic :: ByteString #-}

-- | Code to apply __underline__ emphasis for the terminal output.
underline :: IsString str => str
underline = fromString $ setSGRCode [SetUnderlining SingleUnderline]
{-# SPECIALIZE underline :: String     #-}
{-# SPECIALIZE underline :: Text       #-}
{-# SPECIALIZE underline :: ByteString #-}

{- | Code to apply __double underline__ emphasis for the terminal output.

__⚠ Caution:__ This is not widely supported. It is not natively supported on
Windows 10
-}
doubleUnderline :: IsString str => str
doubleUnderline = fromString $ setSGRCode [SetUnderlining DoubleUnderline]
{-# SPECIALIZE doubleUnderline :: String     #-}
{-# SPECIALIZE doubleUnderline :: Text       #-}
{-# SPECIALIZE doubleUnderline :: ByteString #-}

-- | Code to apply __no underline__ emphasis for the terminal output.
noUnderline :: IsString str => str
noUnderline = fromString $ setSGRCode [SetUnderlining NoUnderline]
{-# SPECIALIZE noUnderline :: String     #-}
{-# SPECIALIZE noUnderline :: Text       #-}
{-# SPECIALIZE noUnderline :: ByteString #-}

-- | Code to indent the terminal output by the given amount of spaces.
indent :: (IsString str, Semigroup str) => Int -> str
indent n
    | n <= 0 = ""
    | otherwise = stimes n " "
{-# SPECIALIZE indent :: Int -> String     #-}
{-# SPECIALIZE indent :: Int -> Text       #-}
{-# SPECIALIZE indent :: Int -> ByteString #-}

-- | Code to reset all previous code applied for the terminal output.
reset :: IsString str => str
reset = fromString $ setSGRCode [Reset]
{-# SPECIALIZE reset :: String     #-}
{-# SPECIALIZE reset :: Text       #-}
{-# SPECIALIZE reset :: ByteString #-}
