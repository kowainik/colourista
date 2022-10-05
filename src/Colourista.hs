{- |
Copyright: (c) 2020-2022 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Improve your terminal output with the handy helpers of @ansi-terminal@ functions
that @colourista@ provides to you.

<<https://user-images.githubusercontent.com/8126674/74609327-0a5dbb00-50e1-11ea-8c4b-2db4ab5b42a2.png Output>>
-}

module Colourista
       ( module Colourista.Pure
         -- $pure
       , module Colourista.IO
         -- $io
       ) where

import Colourista.IO
import Colourista.Pure


{- $pure
The set of pure functions that represents codes to use for the terminal output
customisation: colouring and emphasis.
-}

{- $io
The set of functions that work in 'IO' to output formatted messages
directly to terminal.
-}
