{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.Rewrite.Config
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Config where

import           Compiler.Text
import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Control.Monad.State
import qualified Data.HashMap.Strict as Map
import           Data.Monoid
import           Data.Text           (Text)

rename :: Service f a Shape -> State Config (Service f a Shape)
rename svc = do
    merge renamedTo (f `mapMaybe` Map.toList (svc ^. shapes))
    return svc
  where
    f (k, v)
        | k /= k'   = Just (k, k')
        | otherwise = Nothing
      where
        k' = upperAcronym k

merge :: Setter' Override (Maybe Text)
      -> [(Text, Text)]
      -> State Config ()
merge l xs = modify (typeOverrides %~ replace)
  where
    replace os = Map.fromList (map f xs) <> os
      where
        f (k, v) = (k,) . set l (Just v) $
            case Map.lookup k os of
                Just x  -> x
                Nothing -> Override
                    { _renamedTo      = Nothing
                    , _replacedBy     = Nothing
                    , _enumPrefix     = Nothing
                    , _requiredFields = mempty
                    , _optionalFields = mempty
                    , _renamedFields  = mempty
                    }
