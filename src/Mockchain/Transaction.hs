{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

This module defines 'Transaction', the core type of this package. Each
'Transaction' is 'from' and 'to' a particular 'Address'.

A 'TXID' is 'assign'ed to a 'Transaction' based on its contents and the time
of its creation.

An 'InvalidTransaction' is given by 'validate' when a 'Transaction' fails
basic sanity checks.

-}

module Mockchain.Transaction
    ( Address
    , Transaction (Transaction)
    , TXID, assign
    , InvalidTransaction (..), validate
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson
                     ( FromJSON, FromJSONKey, ToJSON, ToJSONKey, toEncoding
                     , genericToEncoding, defaultOptions
                     )


-- base ----------------------------------------------------------------------
import           Control.Monad (unless)
import           Data.Fixed (Micro)
import           Data.Foldable (fold)
import           Data.Int (Int64)
import           GHC.Generics (Generic)


-- bytestring ----------------------------------------------------------------
import qualified Data.ByteString as B
import           Data.ByteString.Builder
                     ( lazyByteString, toLazyByteString, int64BE
                     )
import qualified Data.ByteString.Lazy as L


-- cryptohash-sha512 ---------------------------------------------------------
import           Crypto.Hash.SHA512 (hashlazy)


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData)


-- hashable ------------------------------------------------------------------
import           Data.Hashable (Hashable)


-- http-api-data -------------------------------------------------------------
import           Web.HttpApiData (FromHttpApiData, ToHttpApiData)


-- monad-validate ------------------------------------------------------------
import           Control.Monad.Validate (ValidateT, dispute)


-- QuickCheck ----------------------------------------------------------------
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen (suchThat)


-- quickcheck-instances ------------------------------------------------------
import           Test.QuickCheck.Instances.Time ()
import           Test.QuickCheck.Instances.UUID ()


-- random --------------------------------------------------------------------
import           System.Random (Random)


-- time ----------------------------------------------------------------------
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)


-- uuid-types ----------------------------------------------------------------
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as U


------------------------------------------------------------------------------
-- | An 'Address' represents the source and destination of a 'Transaction'.
-- It looks like a 'UUID'. You can use the 'Random' instance to generate new
-- ones.
newtype Address = Address UUID
  deriving (Generic)
  deriving newtype
    ( Eq, Ord, Read, Show, Random, Arbitrary
    , NFData, Hashable, FromJSON, ToJSON
    )


------------------------------------------------------------------------------
-- | The core type of this package.
data Transaction = Transaction
    { from :: !Address
    , to :: !Address
    , amount :: !Micro
    , fee :: !Micro
    }
  deriving
    ( Eq, Ord, Read, Show, Generic
    , NFData, Hashable, FromJSON
    )


------------------------------------------------------------------------------
instance ToJSON Transaction where
    toEncoding = genericToEncoding defaultOptions


------------------------------------------------------------------------------
instance Arbitrary Transaction where
    arbitrary = do
        from <- arbitrary
        to <- arbitrary
        valid <- arbitrary
        case valid of
            True -> do
                amount <- arbitrary `suchThat` (> 0)
                fee <- arbitrary `suchThat` \fee ->
                    fee < amount && fee * 20 > amount
                pure $ Transaction from to amount fee
            _ -> Transaction from to <$> arbitrary <*> arbitrary


------------------------------------------------------------------------------
-- | A 'TXID' uniquely identifies a 'Transaction'. It looks like a 'UUID'.
newtype TXID = TXID UUID
  deriving (Generic)
  deriving newtype
    ( Eq, Ord, Read, Show
    , NFData, Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey
    , FromHttpApiData, ToHttpApiData
    )


------------------------------------------------------------------------------
instance Arbitrary TXID where
    arbitrary = assign <$> arbitrary <*> arbitrary


------------------------------------------------------------------------------
-- | Calculate a 'TXID' for a 'Transaction'. The 'TXID' is derived from the
-- on the SHA512 sum of the information in the 'Transaction' and the given
-- 'UTCTime'.
assign :: Transaction -> UTCTime -> TXID
assign (Transaction from to amount fee) time = TXID uuid
  where
    bytes = toLazyByteString $ fold
        [ lazyByteString $ U.toByteString ufrom
        , lazyByteString $ U.toByteString uto
        , int64BE $ unmicro amount
        , int64BE $ unmicro fee
        , int64BE $ unmicro posix
        ]
      where
        Address ufrom = from
        Address uto = to
        posix = utcTimeToPOSIXSeconds time
        unmicro :: RealFrac a => a -> Int64
        unmicro = round . (* 1e6)
    hash = hashlazy bytes
    uuid = maybe U.nil id $ U.fromByteString $ L.fromStrict $ B.take 16 hash


------------------------------------------------------------------------------
data InvalidTransaction = NonPositiveAmount | FeeNotLessThanAmount | FeeTooLow
  deriving
    ( Eq, Ord, Read, Show, Enum, Bounded, Generic
    , NFData, Hashable, FromJSON
    )


------------------------------------------------------------------------------
instance ToJSON InvalidTransaction where
    toEncoding = genericToEncoding defaultOptions


------------------------------------------------------------------------------
-- | Ensures that the transaction meets the requirements for a \"valid\"
-- transaction.
validate :: Monad m => Transaction -> ValidateT [InvalidTransaction] m ()
validate (Transaction _ _ amount fee) = do
    unless (amount > 0) $ dispute [NonPositiveAmount]
    unless (fee < amount) $ dispute [FeeNotLessThanAmount]
    unless (fee * 20 > amount) $ dispute [FeeTooLow]
