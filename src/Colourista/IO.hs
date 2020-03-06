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
      -- ** Aliases
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

import Data.Semigroup ()
import Data.Text (Text)

import qualified Data.Text.IO as TIO

import qualified Colourista.Pure as Colourista

----------------------------------------------------------------------------
-- Direct IO functions
----------------------------------------------------------------------------

-- | Print 'Text' coloured in 'Colourista.red'.
redMessage :: Text -> IO ()
redMessage = formattedMessage [Colourista.red]
{-# INLINE redMessage #-}

-- | Print 'Text' coloured in 'Colourista.green'.
greenMessage :: Text -> IO ()
greenMessage = formattedMessage [Colourista.green]
{-# INLINE greenMessage #-}

-- | Print 'Text' coloured in 'Colourista.blue'.
blueMessage :: Text -> IO ()
blueMessage = formattedMessage [Colourista.blue]
{-# INLINE blueMessage #-}

-- | Print 'Text' coloured in 'Colourista.yellow'.
yellowMessage :: Text -> IO ()
yellowMessage = formattedMessage [Colourista.yellow]
{-# INLINE yellowMessage #-}

-- | Print 'Text' coloured in 'Colourista.black'.
blackMessage :: Text -> IO ()
blackMessage = formattedMessage [Colourista.black]
{-# INLINE blackMessage #-}

-- | Print 'Text' coloured in 'Colourista.white'.
whiteMessage :: Text -> IO ()
whiteMessage = formattedMessage [Colourista.white]
{-# INLINE whiteMessage #-}

-- | Print 'Text' coloured in 'Colourista.magenta'.
magentaMessage :: Text -> IO ()
magentaMessage = formattedMessage [Colourista.magenta]
{-# INLINE magentaMessage #-}

-- | Print 'Text' coloured in 'Colourista.cyan'.
cyanMessage :: Text -> IO ()
cyanMessage = formattedMessage [Colourista.cyan]
{-# INLINE cyanMessage #-}

----------------------------------------------------------------------------
-- Informative aliases
----------------------------------------------------------------------------

-- | Alias for 'greenMessage' that specifies message severity.
successMessage :: Text -> IO ()
successMessage = greenMessage
{-# INLINE successMessage #-}

-- | Alias for 'blueMessage' that specifies message severity.
infoMessage :: Text -> IO ()
infoMessage = blueMessage
{-# INLINE infoMessage #-}

-- | Alias for 'cyanMessage' that specifies message severity.
skipMessage :: Text -> IO ()
skipMessage = cyanMessage
{-# INLINE skipMessage #-}

-- | Alias for 'yellowMessage' that specifies message severity.
warningMessage :: Text -> IO ()
warningMessage = yellowMessage
{-# INLINE warningMessage #-}

-- | Alias for 'redMessage' that specifies message severity.
errorMessage :: Text -> IO ()
errorMessage = redMessage
{-# INLINE errorMessage #-}

----------------------------------------------------------------------------
-- Emphasis
----------------------------------------------------------------------------

-- | Print 'Text' emphasized with 'Colourista.bold'.
boldMessage :: Text -> IO ()
boldMessage = formattedMessage [Colourista.bold]
{-# INLINE boldMessage #-}

-- | Print 'Text' emphasized with 'Colourista.italic'.
italicMessage :: Text -> IO ()
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
formattedMessage :: [Text] -> Text -> IO ()
formattedMessage formatting = TIO.putStrLn . Colourista.formatWith formatting
{-# INLINE formattedMessage #-}
