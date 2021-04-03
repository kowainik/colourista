{- |
Copyright: (c) 2020 Kowainik
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
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (..))
import Data.Text (Text)
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
    :: (HasColourMode, IsString str, Semigroup str)
    => [str]
    -> str
    -> str
formatWith formatting str = case formatting of
    []   -> str
    x:xs -> sconcat (x :| xs) <> str <> reset
{-# SPECIALIZE formatWith :: HasColourMode => [String]     -> String     -> String     #-}
{-# SPECIALIZE formatWith :: HasColourMode => [Text]       -> Text       -> Text       #-}
{-# SPECIALIZE formatWith :: HasColourMode => [ByteString] -> ByteString -> ByteString #-}

----------------------------------------------------------------------------
-- Colours
----------------------------------------------------------------------------

-- | Code to apply 'Red' colouring for the terminal output.
red :: (HasColourMode, IsString str) => str
red = withColourMode $ fromString $ setSGRCode [SetColor Foreground Vivid Red]
{-# SPECIALIZE red :: HasColourMode => String     #-}
{-# SPECIALIZE red :: HasColourMode => Text       #-}
{-# SPECIALIZE red :: HasColourMode => ByteString #-}

-- | Code to apply 'Green' colouring for the terminal output.
green :: (HasColourMode, IsString str) => str
green = withColourMode $ fromString $ setSGRCode [SetColor Foreground Vivid Green]
{-# SPECIALIZE green :: HasColourMode => String     #-}
{-# SPECIALIZE green :: HasColourMode => Text       #-}
{-# SPECIALIZE green :: HasColourMode => ByteString #-}

-- | Code to apply 'Blue' colouring for the terminal output.
blue :: (HasColourMode, IsString str) => str
blue = withColourMode $ fromString $ setSGRCode [SetColor Foreground Vivid Blue]
{-# SPECIALIZE blue :: HasColourMode => String     #-}
{-# SPECIALIZE blue :: HasColourMode => Text       #-}
{-# SPECIALIZE blue :: HasColourMode => ByteString #-}

-- | Code to apply 'Yellow' colouring for the terminal output.
yellow :: (HasColourMode, IsString str) => str
yellow = withColourMode $ fromString $ setSGRCode [SetColor Foreground Vivid Yellow]
{-# SPECIALIZE yellow :: HasColourMode => String     #-}
{-# SPECIALIZE yellow :: HasColourMode => Text       #-}
{-# SPECIALIZE yellow :: HasColourMode => ByteString #-}

-- | Code to apply 'Black' colouring for the terminal output.
black :: (HasColourMode, IsString str) => str
black = withColourMode $ fromString $ setSGRCode [SetColor Foreground Vivid Black]
{-# SPECIALIZE black :: HasColourMode => String     #-}
{-# SPECIALIZE black :: HasColourMode => Text       #-}
{-# SPECIALIZE black :: HasColourMode => ByteString #-}

-- | Code to apply 'White' colouring for the terminal output.
white :: (HasColourMode, IsString str) => str
white = withColourMode $ fromString $ setSGRCode [SetColor Foreground Vivid White]
{-# SPECIALIZE white :: HasColourMode => String     #-}
{-# SPECIALIZE white :: HasColourMode => Text       #-}
{-# SPECIALIZE white :: HasColourMode => ByteString #-}

-- | Code to apply 'Magenta' colouring for the terminal output.
magenta :: (HasColourMode, IsString str) => str
magenta = withColourMode $ fromString $ setSGRCode [SetColor Foreground Vivid Magenta]
{-# SPECIALIZE magenta :: HasColourMode => String     #-}
{-# SPECIALIZE magenta :: HasColourMode => Text       #-}
{-# SPECIALIZE magenta :: HasColourMode => ByteString #-}

-- | Code to apply 'Cyan' colouring for the terminal output.
cyan :: (HasColourMode, IsString str) => str
cyan = withColourMode $ fromString $ setSGRCode [SetColor Foreground Vivid Cyan]
{-# SPECIALIZE cyan :: HasColourMode => String     #-}
{-# SPECIALIZE cyan :: HasColourMode => Text       #-}
{-# SPECIALIZE cyan :: HasColourMode => ByteString #-}

-- | Code to apply any arbitrary hex color for the terminal output.
rgb :: (HasColourMode, IsString str) => String -> Maybe str
rgb hex | length hex > 6        = Nothing
        | length rgbValues /= 3 = Nothing
        | otherwise             =  Just $ withColourMode $ fromString $ setSGRCode [SetRGBColor Foreground (sRGB24 redComponent greenComponent blueComponent)]
  where
    hexVal = replicate (6 - length hex) '0' ++ hex
    rgbValues  = map fromIntegral $ mapMaybe hexToInt [ (hexVal !! 0) : [hexVal !! 1]
                                                      , (hexVal !! 2) : [hexVal !! 3]
                                                      , (hexVal !! 4) : [hexVal !! 5]]

    redComponent   = rgbValues !! 0
    greenComponent = rgbValues !! 1
    blueComponent  = rgbValues !! 2
    hexToInt :: String -> Maybe Int
    hexToInt [] = Just 0
    hexToInt val | isNothing z || isNothing other  = Nothing
                 | otherwise = Just $ fromJust z + 16 * fromJust other
      where
        other = hexToInt (init val)
        z = case last val of
              '0' -> Just 0
              '1' -> Just 1
              '2' -> Just 2
              '3' -> Just 3
              '4' -> Just 4
              '5' -> Just 5
              '6' -> Just 6
              '7' -> Just 7
              '8' -> Just 8
              '9' -> Just 9
              'a' -> Just 10
              'b' -> Just 11
              'c' -> Just 12
              'd' -> Just 13
              'e' -> Just 14
              'f' -> Just 15
              'A' -> Just 10
              'B' -> Just 11
              'C' -> Just 12
              'D' -> Just 13
              'E' -> Just 14
              'F' -> Just 15
              _   -> Nothing

----------------------------------------------------------------------------
-- Background
----------------------------------------------------------------------------

-- | Code to apply 'Red' background colouring for the terminal output.
redBg :: (HasColourMode, IsString str) => str
redBg = withColourMode $ fromString $ setSGRCode [SetColor Background Vivid Red]
{-# SPECIALIZE redBg :: HasColourMode => String     #-}
{-# SPECIALIZE redBg :: HasColourMode => Text       #-}
{-# SPECIALIZE redBg :: HasColourMode => ByteString #-}

-- | Code to apply 'Green' background colouring for the terminal output.
greenBg :: (HasColourMode, IsString str) => str
greenBg = withColourMode $ fromString $ setSGRCode [SetColor Background Vivid Green]
{-# SPECIALIZE greenBg :: HasColourMode => String     #-}
{-# SPECIALIZE greenBg :: HasColourMode => Text       #-}
{-# SPECIALIZE greenBg :: HasColourMode => ByteString #-}

-- | Code to apply 'Blue' background colouring for the terminal output.
blueBg :: (HasColourMode, IsString str) => str
blueBg = withColourMode $ fromString $ setSGRCode [SetColor Background Vivid Blue]
{-# SPECIALIZE blueBg :: HasColourMode => String     #-}
{-# SPECIALIZE blueBg :: HasColourMode => Text       #-}
{-# SPECIALIZE blueBg :: HasColourMode => ByteString #-}

-- | Code to apply 'Yellow' background colouring for the terminal output.
yellowBg :: (HasColourMode, IsString str) => str
yellowBg = withColourMode $ fromString $ setSGRCode [SetColor Background Vivid Yellow]
{-# SPECIALIZE yellowBg :: HasColourMode => String     #-}
{-# SPECIALIZE yellowBg :: HasColourMode => Text       #-}
{-# SPECIALIZE yellowBg :: HasColourMode => ByteString #-}

-- | Code to apply 'Black' background colouring for the terminal output.
blackBg :: (HasColourMode, IsString str) => str
blackBg = withColourMode $ fromString $ setSGRCode [SetColor Background Vivid Black]
{-# SPECIALIZE blackBg :: HasColourMode => String     #-}
{-# SPECIALIZE blackBg :: HasColourMode => Text       #-}
{-# SPECIALIZE blackBg :: HasColourMode => ByteString #-}

-- | Code to apply 'White' background colouring for the terminal output.
whiteBg :: (HasColourMode, IsString str) => str
whiteBg = withColourMode $ fromString $ setSGRCode [SetColor Background Vivid White]
{-# SPECIALIZE whiteBg :: HasColourMode => String     #-}
{-# SPECIALIZE whiteBg :: HasColourMode => Text       #-}
{-# SPECIALIZE whiteBg :: HasColourMode => ByteString #-}

-- | Code to apply 'Magenta' background colouring for the terminal output.
magentaBg :: (HasColourMode, IsString str) => str
magentaBg = withColourMode $ fromString $ setSGRCode [SetColor Background Vivid Magenta]
{-# SPECIALIZE magentaBg :: HasColourMode => String     #-}
{-# SPECIALIZE magentaBg :: HasColourMode => Text       #-}
{-# SPECIALIZE magentaBg :: HasColourMode => ByteString #-}

-- | Code to apply 'Cyan' background colouring for the terminal output.
cyanBg :: (HasColourMode, IsString str) => str
cyanBg = withColourMode $ fromString $ setSGRCode [SetColor Background Vivid Cyan]
{-# SPECIALIZE cyanBg :: HasColourMode => String     #-}
{-# SPECIALIZE cyanBg :: HasColourMode => Text       #-}
{-# SPECIALIZE cyanBg :: HasColourMode => ByteString #-}

----------------------------------------------------------------------------
-- Emphasis
----------------------------------------------------------------------------

-- | Code to apply __bold__ emphasis for the terminal output.
bold :: (HasColourMode, IsString str) => str
bold = withColourMode $ fromString $ setSGRCode [SetConsoleIntensity BoldIntensity]
{-# SPECIALIZE bold :: HasColourMode => String     #-}
{-# SPECIALIZE bold :: HasColourMode => Text       #-}
{-# SPECIALIZE bold :: HasColourMode => ByteString #-}

-- | Code to apply /italic/ emphasis for the terminal output.
italic :: (HasColourMode, IsString str) => str
italic = withColourMode $ fromString $ setSGRCode [SetItalicized True]
{-# SPECIALIZE italic :: HasColourMode => String     #-}
{-# SPECIALIZE italic :: HasColourMode => Text       #-}
{-# SPECIALIZE italic :: HasColourMode => ByteString #-}

-- | Code to apply __underline__ emphasis for the terminal output.
underline :: (HasColourMode, IsString str) => str
underline = withColourMode $ fromString $ setSGRCode [SetUnderlining SingleUnderline]
{-# SPECIALIZE underline :: HasColourMode => String     #-}
{-# SPECIALIZE underline :: HasColourMode => Text       #-}
{-# SPECIALIZE underline :: HasColourMode => ByteString #-}

{- | Code to apply __double underline__ emphasis for the terminal output.

__⚠ Caution:__ This is not widely supported. It is not natively supported on
Windows 10
-}
doubleUnderline :: (HasColourMode, IsString str) => str
doubleUnderline = withColourMode $ fromString $ setSGRCode [SetUnderlining DoubleUnderline]
{-# SPECIALIZE doubleUnderline :: HasColourMode => String     #-}
{-# SPECIALIZE doubleUnderline :: HasColourMode => Text       #-}
{-# SPECIALIZE doubleUnderline :: HasColourMode => ByteString #-}

-- | Code to apply __no underline__ emphasis for the terminal output.
noUnderline :: (HasColourMode, IsString str) => str
noUnderline = withColourMode $ fromString $ setSGRCode [SetUnderlining NoUnderline]
{-# SPECIALIZE noUnderline :: HasColourMode => String     #-}
{-# SPECIALIZE noUnderline :: HasColourMode => Text       #-}
{-# SPECIALIZE noUnderline :: HasColourMode => ByteString #-}

-- | Code to indent the terminal output by the given amount of spaces.
indent :: (IsString str, Semigroup str) => Int -> str
indent n
    | n <= 0 = ""
    | otherwise = stimes n " "
{-# SPECIALIZE indent :: Int -> String     #-}
{-# SPECIALIZE indent :: Int -> Text       #-}
{-# SPECIALIZE indent :: Int -> ByteString #-}

-- | Code to reset all previous code applied for the terminal output.
reset :: (HasColourMode, IsString str) => str
reset = withColourMode $ fromString $ setSGRCode [Reset]
{-# SPECIALIZE reset :: HasColourMode => String     #-}
{-# SPECIALIZE reset :: HasColourMode => Text       #-}
{-# SPECIALIZE reset :: HasColourMode => ByteString #-}
