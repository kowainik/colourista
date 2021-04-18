{-# LANGUAGE CPP #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions to output formatted 'T.Text' directly to terminal.
-}

module Colourista.IO
    ( -- * Color
      -- ** Direct
      redMessage
    , greenMessage
    , blueMessage
    , yellowMessage
    , blackMessage
    , whiteMessage
    , magentaMessage
    , cyanMessage
    , rgbMessage
      -- ** Aliases with unicode indicators
    , successMessage
    , infoMessage
    , skipMessage
    , warningMessage
    , errorMessage
      -- * Emphasis
    , boldMessage
    , italicMessage
      -- * General purpose
    , formattedMessage
    ) where

#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup (..))
#endif
import Data.Text (Text)
import Data.Word (Word8)
import Colourista.Mode (HasColourMode)

import qualified Data.Text.IO as TIO

import qualified Colourista.Pure as Colourista

----------------------------------------------------------------------------
-- Direct IO functions
----------------------------------------------------------------------------

-- | Print 'Text' coloured in specified RGB notaion
rgbMessage :: Word8 -> Word8 -> Word8 -> Text -> IO ()
rgbMessage  red green blue = formattedMessage [ resColor ]
  where resColor = Colourista.rgb red green blue

-- | Print 'Text' coloured in 'Colourista.red'.
redMessage :: HasColourMode => Text -> IO ()
redMessage = formattedMessage [Colourista.red]
{-# INLINE redMessage #-}

-- | Print 'Text' coloured in 'Colourista.green'.
greenMessage :: HasColourMode => Text -> IO ()
greenMessage = formattedMessage [Colourista.green]
{-# INLINE greenMessage #-}

-- | Print 'Text' coloured in 'Colourista.blue'.
blueMessage :: HasColourMode => Text -> IO ()
blueMessage = formattedMessage [Colourista.blue]
{-# INLINE blueMessage #-}

-- | Print 'Text' coloured in 'Colourista.yellow'.
yellowMessage :: HasColourMode => Text -> IO ()
yellowMessage = formattedMessage [Colourista.yellow]
{-# INLINE yellowMessage #-}

-- | Print 'Text' coloured in 'Colourista.black'.
blackMessage :: HasColourMode => Text -> IO ()
blackMessage = formattedMessage [Colourista.black]
{-# INLINE blackMessage #-}

-- | Print 'Text' coloured in 'Colourista.white'.
whiteMessage :: HasColourMode => Text -> IO ()
whiteMessage = formattedMessage [Colourista.white]
{-# INLINE whiteMessage #-}

-- | Print 'Text' coloured in 'Colourista.magenta'.
magentaMessage :: HasColourMode => Text -> IO ()
magentaMessage = formattedMessage [Colourista.magenta]
{-# INLINE magentaMessage #-}

-- | Print 'Text' coloured in 'Colourista.cyan'.
cyanMessage :: HasColourMode => Text -> IO ()
cyanMessage = formattedMessage [Colourista.cyan]
{-# INLINE cyanMessage #-}

----------------------------------------------------------------------------
-- Informative aliases
----------------------------------------------------------------------------

{- | Similar to 'greenMessage', but add unicode indicator.

<<https://user-images.githubusercontent.com/4276606/80867598-dbd99000-8c8c-11ea-9fac-81a1a606d8d8.png Success message>>
-}
successMessage :: HasColourMode => Text -> IO ()
successMessage t = greenMessage $ "  ✔ " <> t
{-# INLINE successMessage #-}

{- | Similar to 'blueMessage', but add unicode indicator.

<<https://user-images.githubusercontent.com/4276606/80867597-db40f980-8c8c-11ea-9775-e8a3c4a7aaa2.png Information message>>
-}
infoMessage :: HasColourMode => Text -> IO ()
infoMessage t = blueMessage $ "  ⓘ " <> t
{-# INLINE infoMessage #-}

{- | Similar to 'cyanMessage', but add unicode indicator.

<<https://user-images.githubusercontent.com/4276606/80867596-db40f980-8c8c-11ea-8131-9c7cba32a4fd.png Skip message>>
-}
skipMessage :: HasColourMode => Text -> IO ()
skipMessage t = cyanMessage $ "  ▶ " <> t
{-# INLINE skipMessage #-}

{- | Similar to 'yellowMessage', but add unicode indicator.

<<https://user-images.githubusercontent.com/4276606/80867594-daa86300-8c8c-11ea-9c6a-a42b634a1e4b.png Warning message>>
-}
warningMessage :: HasColourMode => Text -> IO ()
warningMessage t = yellowMessage $ "  ⚠ " <> t
{-# INLINE warningMessage #-}

{- | Similar to 'redMessage', but add unicode indicator.

<<https://user-images.githubusercontent.com/4276606/80867592-da0fcc80-8c8c-11ea-90e0-42aae8770c18.png Error message>>
-}
errorMessage :: HasColourMode => Text -> IO ()
errorMessage t = redMessage $ "  \128721 " <> t
{-# INLINE errorMessage #-}

----------------------------------------------------------------------------
-- Emphasis
----------------------------------------------------------------------------

-- | Print 'Text' emphasized with 'Colourista.bold'.
boldMessage :: HasColourMode => Text -> IO ()
boldMessage = formattedMessage [Colourista.bold]
{-# INLINE boldMessage #-}

-- | Print 'Text' emphasized with 'Colourista.italic'.
italicMessage :: HasColourMode => Text -> IO ()
italicMessage = formattedMessage [Colourista.italic]
{-# INLINE italicMessage #-}

----------------------------------------------------------------------------
-- General purposes
----------------------------------------------------------------------------

{- | Print message with specified list of formatting options. See
'Colourista.formatWith' for more details. If this function takes empty
list, no formatting is applied.

![formattedMessage-example](https://user-images.githubusercontent.com/4276606/74608898-e6987600-50dc-11ea-9a93-bda701fd3c43.png)
-}
formattedMessage :: HasColourMode => [Text] -> Text -> IO ()
formattedMessage formatting = TIO.putStrLn . Colourista.formatWith formatting
{-# INLINE formattedMessage #-}
