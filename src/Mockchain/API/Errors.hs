{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Mockchain.API.Errors
    ( invalidate, maybe404
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson
                     ( FromJSON, ToJSON, toEncoding, encode
                     , genericToEncoding, defaultOptions
                     )


-- base ----------------------------------------------------------------------
import           GHC.Generics (Generic)


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData)


-- hashable ------------------------------------------------------------------
import           Data.Hashable (Hashable)


-- monad-validate ------------------------------------------------------------
import           Control.Monad.Validate (Validate, runValidate)


-- mtl -----------------------------------------------------------------------
import           Control.Monad.Error.Class (MonadError, throwError)


-- servant-server ------------------------------------------------------------
import           Servant.Server (ServerError (..), err400, err404)


------------------------------------------------------------------------------
-- | A simple wrapper type, the 'ToJSON' instance of which formats the
-- response body how we want.
newtype Errors a = Errors {errors :: a}
  deriving
    ( Eq, Ord, Read, Show, Generic
    , NFData, Hashable, FromJSON
    )


------------------------------------------------------------------------------
instance ToJSON a => ToJSON (Errors a) where
    toEncoding = genericToEncoding defaultOptions


------------------------------------------------------------------------------
-- | Given a validation to run, run it, and if it fails, display a nice
-- JSON-formatted list of reasons it failed with an appropriate status code.
invalidate :: (ToJSON e, MonadError ServerError m) => Validate e a -> m a
invalidate validator = case runValidate validator of
    Left errors -> throwError err400
        { errBody = encode $ Errors errors
        , errHeaders = [("Content-Type", "application/json")]
        }
    Right a -> pure a


------------------------------------------------------------------------------
maybe404 :: MonadError ServerError m => Maybe a -> m a
maybe404 = maybe (throwError err404) pure
