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

import Data.Semigroup ((<>))
import Data.Text (Text)

import qualified Data.Text.IO as TIO

import qualified Colourista.Pure as Colourista

----------------------------------------------------------------------------
-- Direct IO functions
----------------------------------------------------------------------------

-- | Print 'Text' coloured in 'Colourista.red'.
redMessage :: Text -> IO ()
redMessage = formattedMessage Colourista.red

-- | Print 'Text' coloured in 'Colourista.green'.
greenMessage :: Text -> IO ()
greenMessage = formattedMessage Colourista.green

-- | Print 'Text' coloured in 'Colourista.blue'.
blueMessage :: Text -> IO ()
blueMessage = formattedMessage Colourista.blue

-- | Print 'Text' coloured in 'Colourista.yellow'.
yellowMessage :: Text -> IO ()
yellowMessage = formattedMessage Colourista.yellow

-- | Print 'Text' coloured in 'Colourista.black'.
blackMessage :: Text -> IO ()
blackMessage = formattedMessage Colourista.black

-- | Print 'Text' coloured in 'Colourista.white'.
whiteMessage :: Text -> IO ()
whiteMessage = formattedMessage Colourista.white

-- | Print 'Text' coloured in 'Colourista.magenta'.
magentaMessage :: Text -> IO ()
magentaMessage = formattedMessage Colourista.magenta

-- | Print 'Text' coloured in 'Colourista.cyan'.
cyanMessage :: Text -> IO ()
cyanMessage = formattedMessage Colourista.cyan

----------------------------------------------------------------------------
-- Informative aliases
----------------------------------------------------------------------------

-- | Alias for 'greenMessage' that specifies message severity.
successMessage :: Text -> IO ()
successMessage = greenMessage

-- | Alias for 'blueMessage' that specifies message severity.
infoMessage :: Text -> IO ()
infoMessage = blueMessage

-- | Alias for 'cyanMessage' that specifies message severity.
skipMessage :: Text -> IO ()
skipMessage = cyanMessage

-- | Alias for 'yellowMessage' that specifies message severity.
warningMessage :: Text -> IO ()
warningMessage = yellowMessage

-- | Alias for 'redMessage' that specifies message severity.
errorMessage :: Text -> IO ()
errorMessage = redMessage

----------------------------------------------------------------------------
-- Emphasis
----------------------------------------------------------------------------

-- | Print 'Text' emphasized with 'Colourista.bold'.
boldMessage :: Text -> IO ()
boldMessage = formattedMessage Colourista.bold

-- | Print 'Text' emphasized with 'Colourista.italic'.
italicMessage :: Text -> IO ()
italicMessage = formattedMessage Colourista.italic

----------------------------------------------------------------------------
-- General purposes
----------------------------------------------------------------------------

{- | Print message with specified formatting, either colored or
emphasized.
-}
formattedMessage :: Text -> Text -> IO ()
formattedMessage formatting message =
    TIO.putStrLn $ formatting <> message <> Colourista.reset
