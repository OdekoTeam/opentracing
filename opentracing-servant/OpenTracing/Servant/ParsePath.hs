{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

module OpenTracing.Servant.ParsePath
  ( ParsePath(..)
  , pathDescription
  ) where


import Control.Applicative
import Data.Function ((&))
import Servant
  ( FromHttpApiData(..), Capture, (:>), Verb, Proxy(..), (:<|>)
  , ReqBody', Description, QueryParam', QueryParams
  )
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits
import Data.Vault.Lazy (Vault)


pathDescription :: ParsePath api => Proxy api -> [Text] -> Maybe Text
pathDescription p xs = do
  desc <- parsePathDescription p xs
  case desc of
    "" -> return "/"
    _ -> return desc

class ParsePath api where
  parsePathDescription :: Proxy api -> [Text] -> Maybe Text

instance ParsePath (Verb method status ctypes a) where
  parsePathDescription _ [] = Just ""
  parsePathDescription _ _ = Nothing -- don't accept the path if pieces are left over

instance (KnownSymbol path, ParsePath api) => ParsePath (path :> api)  where
  parsePathDescription _ (x:xs)
    | x == T.pack (symbolVal $ Proxy @path) = parsePathDescription (Proxy @api) xs
        & fmap (\rest -> "/" <> T.pack (symbolVal $ Proxy @path) <> rest)
  parsePathDescription _ _ = Nothing

instance (ParsePath api, KnownSymbol capture, FromHttpApiData a) => ParsePath (Capture capture a :> api) where
  parsePathDescription _ (x:xs)
    | Right _ <- parseUrlPiece @a x = parsePathDescription (Proxy @api) xs
        & fmap (\rest -> "/:" <> T.pack (symbolVal $ Proxy @capture) <> rest)
  parsePathDescription _ _ = Nothing

instance (ParsePath api) => ParsePath (Vault :> api) where
  parsePathDescription _ xs = parsePathDescription (Proxy @api) xs

instance (ParsePath l, ParsePath r) => ParsePath (l :<|> r) where
  parsePathDescription _ xs = parsePathDescription (Proxy @l) xs <|> parsePathDescription (Proxy @r) xs

instance (ParsePath api) => ParsePath (ReqBody' x y z :> api) where
  parsePathDescription _ xs = parsePathDescription (Proxy @api) xs

instance (ParsePath api) => ParsePath (Description t :> api) where
  parsePathDescription _ xs = parsePathDescription (Proxy @api) xs

instance (ParsePath api) => ParsePath (QueryParam' x y z :> api) where
  parsePathDescription _ xs = parsePathDescription (Proxy @api) xs

instance (ParsePath api) => ParsePath (QueryParams y z :> api) where
  parsePathDescription _ xs = parsePathDescription (Proxy @api) xs
