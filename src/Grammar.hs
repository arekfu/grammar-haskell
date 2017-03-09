{-|
Module      : Grammar
Description : Reexport the most useful functions in the Grammar submodules.
Copyright   : (c) Davide Mancusi, 2017
License     : BSD
Maintainer  : arekfu@gmail.com
Stability   : experimental
Portability : POSIX

This module just reexports the 'Grammar.Internal' and 'Grammar.Random' modules.
-}
module Grammar
( module Grammar.Internal
, module Grammar.Random
) where

-- local imports
import Grammar.Internal
import Grammar.Random
