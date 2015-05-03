{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Compiler.Rewrite.TypeOf
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.TypeOf
    ( annotateTypes
    ) where

import           Compiler.Formatting
import           Compiler.Rewrite.Prefix
import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Control.Monad.State
import qualified Data.HashMap.Strict          as Map
import           Data.Monoid                  hiding (Product, Sum)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as Build
import           HIndent
import           Language.Haskell.Exts        hiding (Int, List, Lit)
import           Language.Haskell.Exts.SrcLoc (noLoc)

annotateTypes :: Monad m
              => Service Identity Shape Shape
              -> Compiler m (Service Identity Data Data)
annotateTypes svc@Service{..} = do
    ps <- getPrefixes _shapes

    let f x = Map.fromList
            [ (x ^. requestName,  x ^. opInput  . _Identity)
            , (x ^. responseName, x ^. opOutput . _Identity)
            ]

    ts <- solve (_shapes <> foldMap f _operations)
    ss <- kvTraverseMaybe (datatype ps ts) _shapes

    return $! svc
        { _operations = mempty
        , _shapes     = ss
        }

-- FIXME:
-- It'll be an error if the operation input/output is untranslatable
--
-- Should empty responses be a shared type, and always succeed based
-- on HTTP response code?

solve :: Monad m => Map Text (Shape f) -> Compiler m (Map Text Type)
solve ss = evalStateT (Map.traverseWithKey go ss) mempty
  where
    go :: Monad m
       => Text
       -> Shape f
       -> StateT (Map Text Type) (Compiler m) Type
    go n = \case
        Struct {}  -> save n (tycon n)
        Enum   {}  -> save n (tycon n)

        List _ e -> do
            t <- TyApp (tycon "List") <$> memo (e ^. refShape)
            save n t

        Map _ k v -> do
            t <- TyApp <$> memo (k ^. refShape) <*> memo (v ^. refShape)
            save n (TyApp (tycon "Map") t)

        Lit _ l -> case l of
            Int    -> save n (tycon "Int")
            Long   -> save n (tycon "Long")
            Double -> save n (tycon "Double")
            Text   -> save n (tycon "Text")
            Blob   -> save n (tycon "Blob")
            Time   -> save n (tycon "Time")
            Bool   -> save n (tycon "Bool")

    save :: Monad m => Text -> Type -> StateT (Map Text Type) m Type
    save n t = modify (Map.insert n t) >> return t

    memo :: Monad m
         => Text
         -> StateT (Map Text Type) (Compiler m) Type
    memo n = do
        m <- gets (Map.lookup n)
        case m of
            Just x  -> return x
            Nothing -> do
                case Map.lookup n ss of
                    Just x  -> go n x
                    Nothing -> failure ("Unable to find type: " % stext) n

datatype :: Monad m
         => Map Text Text
         -> Map Text Type
         -> Text
         -> Shape Identity
         -> Compiler m (Maybe (Data Identity))
datatype ps ts n = \case
    Struct i s -> hoistEither $ Just <$> (note (LText.pack $ show (n, ps)) (Map.lookup n ps) >>= prod i s)
    Enum   {}  -> return Nothing
    _          -> return Nothing
  where
    prod i s@Struct'{..} (Text.toLower -> p) = Product i s
        <$> decl
        <*> pure []
        <*> ctor
        <*> pure lenses
      where
        decl = do
           fs <- traverse field (Map.toList _members)
           pretty $ DataDecl noLoc arity [] (ident n) []
               [ QualConDecl noLoc [] [] (RecDecl (ident n) fs)
               ] []

        -- Facets of Info for the field need to be layered on top
        -- of the type, such as nonempty, maybe, etc.
        field (k, v) = do
            ([ident ("_" <> p <> k)],) <$> note m (Map.lookup t ts)
          where
            t = v ^. refShape
            m = format ("Missing type " % stext %
                        " of field "    % stext %
                        " in record "   % stext)
                       t k n

        arity | Map.size _members == 1 = NewType
              | otherwise              = DataType

        ctor = pure undefined -- Fun

        lenses = mempty

pretty :: Decl -> Either LazyText LazyText
pretty d = bimap e Build.toLazyText $ reformat johanTibell Nothing p
  where
    e = flip mappend (" - when formatting datatype: " <> p) . LText.pack

    p = LText.pack (prettyPrintStyleMode s m d)

    s = style
        { mode           = PageMode
        , lineLength     = 80
        , ribbonsPerLine = 1.5
        }

    m = defaultMode
        { spacing = False
        , layout  = PPNoLayout
        }

tycon :: Text -> Type
tycon = TyCon . unqual

unqual :: Text -> QName
unqual = UnQual . ident

ident :: Text -> Name
ident = Ident . Text.unpack
