{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}

module OpenTracing.Servant where

import           Control.Applicative
import           Control.Lens            (over, set, view)
import           Data.Maybe
import           Data.Proxy
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text               as Text
import           Data.Text.Encoding      (decodeUtf8)
import           Data.Vault.Lazy (Key)
import qualified Data.Vault.Lazy as V
import           Network.Wai
import           OpenTracing
import           OpenTracing.Servant.ParsePath
import qualified OpenTracing.Propagation as Propagation
import qualified OpenTracing.Tracer      as Tracer
import           Prelude                 hiding (span)

type TracedApplication = ActiveSpan -> Application

parentSpanKey :: IO (Key ActiveSpan)
parentSpanKey = V.newKey

data OpenTracingEnv api p = OpenTracingEnv
  { opentracingAPIProxy :: Proxy api
  , opentracingActiveSpanKey :: Key ActiveSpan
  , opentracingTracer :: Tracer
  , opentracingPropagation :: Propagation p
  , opentracingResourceTag :: Maybe (Text -> Tag)
  }

opentracing
    :: (HasCarrier Headers p, ParsePath api)
    => OpenTracingEnv api p
    -> TracedApplication
    -> Application
opentracing env app req respond = do
    let propagation_ = opentracingPropagation env
        api = opentracingAPIProxy env
        tracer_ = opentracingTracer env
        vaultKey = opentracingActiveSpanKey env
        ctx = Propagation.extract propagation_ (requestHeaders req)
        opt = let name = "servant.request"
                  resource = pathDescription api $ pathInfo req
                  refs = (\x -> set refPropagated x mempty)
                       . maybeToList . fmap ChildOf $ ctx
               in set spanOptSampled (view ctxSampled <$> ctx)
                . set spanOptTags
                      ([ HttpMethod  (requestMethod req)
                       , HttpUrl     (decodeUtf8 url)
                       , PeerAddress (Text.pack (show (remoteHost req))) -- not so great
                       , SpanKind    RPCServer
                       ] ++ catMaybes
                       [ opentracingResourceTag env <*> resource
                       ])
                $ spanOpts name refs

    Tracer.traced_ tracer_ opt $ \span -> do
      let oldVault = vault req
          newVault = V.insert vaultKey span oldVault
          newReq = req { vault = newVault }
      app span newReq $ \res -> do
        modifyActiveSpan span $
            over spanTags (setTag (HttpStatusCode (responseStatus res)))
        respond res
  where
    url = "http" <> if isSecure req then "s" else mempty <> "://"
       <> fromMaybe "localhost" (requestHeaderHost req)
       <> rawPathInfo req <> rawQueryString req
