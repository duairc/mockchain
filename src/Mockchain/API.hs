{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Mockchain.API
    ( Mockchain (..), api, mockchain
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad ((>=>))
import           Data.Proxy (Proxy (Proxy))
import           GHC.Generics (Generic)


-- mockchain -----------------------------------------------------------------
import           Mockchain.API.Errors (maybe404, invalidate)
import           Mockchain.Server (Result)
import qualified Mockchain.Server as M
import           Mockchain.Transaction (Transaction, TXID, validate)


-- network-uri ---------------------------------------------------------------
import           Network.URI (uriToString)


-- servant -------------------------------------------------------------------
import           Servant.API
                     ( (:>), Capture, JSON, Get, PostCreated, ReqBody
                     , Headers, Header, addHeader
                     )
import           Servant.API.Generic ((:-), ToServantApi, genericApi)
import           Servant.Links (fieldLink, linkURI)


-- servant-server ------------------------------------------------------------
import           Servant.Server (Handler)
import           Servant.Server.Generic (AsServerT)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))


------------------------------------------------------------------------------
type App = ReaderT M.Server Handler


------------------------------------------------------------------------------
data Mockchain route = Mockchain
    { broadcast :: route :- ReqBody '[JSON] Transaction
        :> PostCreated '[JSON] (Headers '[Header "Location" String] TXID)
    , get :: route :- Capture "txid" TXID :> Get '[JSON] Result
    }
  deriving (Generic)


------------------------------------------------------------------------------
api :: Proxy (ToServantApi Mockchain)
api = genericApi (Proxy :: Proxy Mockchain)


------------------------------------------------------------------------------
mockchain :: Mockchain (AsServerT App)
mockchain = Mockchain broadcast _get
  where
    broadcast transaction = do
        invalidate $ validate transaction
        ReaderT $ \server -> do
            txid <- M.broadcast transaction server
            let link = uriToString id (linkURI $ fieldLink get txid) mempty
            pure $ addHeader ('/' : link) txid
    _get txid = ReaderT $ M.get txid >=> maybe404
