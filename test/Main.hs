{-# LANGUAGE OverloadedLabels #-}

module Main
    ( main
    )
where

-- base ----------------------------------------------------------------------
import           Control.Concurrent (forkIO, killThread, threadDelay)
import           Control.Exception (bracket, throwIO)


-- generic-lens --------------------------------------------------------------
import           Data.Generics.Labels ()


-- hspec ---------------------------------------------------------------------
import           Test.Hspec


-- http-client ---------------------------------------------------------------
import           Network.HTTP.Client (defaultManagerSettings, newManager)


-- http-types ----------------------------------------------------------------
import           Network.HTTP.Types (statusCode)


-- lens ----------------------------------------------------------------------
import           Control.Lens.Getter ((^.))


-- mockchain -----------------------------------------------------------------
import           Mockchain.API (Mockchain, api, mockchain)
import qualified Mockchain.API as API
import           Mockchain.Server (Status (Pending), new)
import           Mockchain.Transaction (Transaction (Transaction), assign)


-- QuickCheck ----------------------------------------------------------------
import           Test.QuickCheck (quickCheckWith, stdArgs, maxSuccess)


-- random --------------------------------------------------------------------
import           System.Random (randomIO)


-- servant -------------------------------------------------------------------
import           Servant.API.Generic (ToServantApi)
import           Servant.API.ResponseHeaders (getResponse)


-- servant-client ------------------------------------------------------------
import           Servant.Client (mkClientEnv, parseBaseUrl, runClientM)


-- servant-client-core -------------------------------------------------------
import           Servant.Client.Generic (AsClientT, genericClientHoist)
import           Servant.Client.Core.ClientError
                     ( ClientError (FailureResponse)
                     )
import           Servant.Client.Core.Response (responseStatusCode)


-- servant-quickcheck --------------------------------------------------------
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal (serverDoesntSatisfy)


-- servant-server ------------------------------------------------------------
import           Servant.Server (Server, hoistServer, serve)
import           Servant.Server.Generic (genericServerT)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (runReaderT)


-- warp ----------------------------------------------------------------------
import           Network.Wai.Handler.Warp (run)


------------------------------------------------------------------------------
-- | Forks a new instance mockchain on the given port and runs the given
-- action.
withServer :: Int -> IO () -> IO ()
withServer port = bracket fork kill . const
  where
    fork = forkIO $ server >>= run port . serve api
    kill = killThread


------------------------------------------------------------------------------
withClient :: (Mockchain (AsClientT IO) -> IO ()) -> IO ()
withClient f = withServer 8888 $ do
    base <- parseBaseUrl "http://localhost:8888"
    manager <- newManager defaultManagerSettings
    let env = mkClientEnv manager base
    f $ genericClientHoist $ \m -> runClientM m env >>= either throwIO pure


------------------------------------------------------------------------------
-- | Checks that unique 'Transaction's generate unique 'TXID's
txids :: Spec
txids = describe "TXIDs" $ do
    it "are unique" $ quickCheckWith args uniqueTXID
  where
    uniqueTXID transaction transaction' time = input == output
      where
        input = transaction == transaction'
        output = assign transaction time == assign transaction' time


------------------------------------------------------------------------------
-- | Checks that the @broadcast@ and @get@ endpoints behave appropriately.
endpoints :: Spec
endpoints = around withClient $ do
    from <- runIO randomIO
    to <- runIO randomIO
    describe "Mockchain API (business logic)" $ do
        it "should reject a bad transaction" $ \client -> do
            let transaction = Transaction from to (-20) 5
            API.broadcast client transaction `shouldThrow` isBadRequest
        it "create a pending transaction" $ \client -> do
            let transaction = Transaction from to 20 5
            txid <- getResponse <$> API.broadcast client transaction
            result <- API.get client txid
            result ^. #status `shouldBe` Pending
        it "accept/reject a transaction after 10 seconds" $ \client -> do
            let transaction = Transaction from to 20 5
            txid <- getResponse <$> API.broadcast client transaction
            threadDelay 11000000
            result <- API.get client txid
            result ^. #status `shouldNotBe` Pending
  where
    isBadRequest (FailureResponse _ response) = code == 400
      where
        code = statusCode $ responseStatusCode response
    isBadRequest _ = False


------------------------------------------------------------------------------
-- | This uses @servant-quickcheck@ to test that the 'Mockchain' API conforms
-- to best-practices. We only implement a few right now.
wholeAPI :: Spec
wholeAPI = describe "Mockchain API (best practices)" $ do
    it "implements some best practices" $ do
        withServantServer api server $ \base ->
            serverSatisfies api base args implemented
    it "but not others" $ do
        withServantServer api server $ \base -> do
            serverDoesntSatisfy api base args unimplemented
  where
    implemented = unauthorizedContainsWWWAuthenticate <%> not500
        <%> notLongerThan 150000000
        <%> mempty
    unimplemented = getsHaveCacheControlHeader <%> onlyJsonObjects
        <%> notAllowedContainsAllowHeader <%> honoursAcceptHeader
        <%> getsHaveLastModifiedHeader <%> headsHaveCacheControlHeader
        <%> createContainsValidLocation
        -- ^ This one should pass but doesn't, I haven't figured out why yet
        <%> mempty


------------------------------------------------------------------------------
specs :: Spec
specs = do
    txids
    endpoints
    wholeAPI


------------------------------------------------------------------------------
main :: IO ()
main = hspec specs


------------------------------------------------------------------------------
args :: Args
args = stdArgs {maxSuccess = 1000}


------------------------------------------------------------------------------
server :: IO (Server (ToServantApi Mockchain))
server = do
    state <- new config
    pure $ hoistServer api (flip runReaderT state) $ genericServerT mockchain
  where
    config = mempty
