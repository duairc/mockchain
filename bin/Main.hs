module Main
    ( main
    )
where

-- mockchain -----------------------------------------------------------------
import           Mockchain.API (mockchain)
import           Mockchain.Server (Config (Config), new)


-- optparse-applicative ------------------------------------------------------
import           Options.Applicative (ParserInfo, execParser)
import qualified Options.Applicative as O


-- servant-server ------------------------------------------------------------
import           Servant.Server.Generic (genericServeT)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (runReaderT)


-- warp ----------------------------------------------------------------------
import           Network.Wai.Handler.Warp (runEnv)


------------------------------------------------------------------------------
parse :: ParserInfo Config
parse = O.info parser (O.fullDesc <> O.progDesc description)
  where
    description = "run a mockchain API server"
    parser = O.helper <*> go
    go = Config <$> timeout <*> threshold
    timeout = O.option difftime $ O.long "timeout" <> O.metavar "SECONDS"
        <> O.value dtimeout
        <> O.help "Reject transactions older than (default 300)"
    threshold = O.option O.auto $ O.long "threshold" <> O.metavar "RATIO"
        <> O.value dthreshold
        <> O.help "Reject transactions below fee/amount ratio (default 0.1)"
    -- no Read instance for NominalDiffTime
    difftime = realToFrac <$> (O.auto :: O.ReadM Double)
    -- defaults
    Config dtimeout dthreshold = mempty


------------------------------------------------------------------------------
run :: Config -> IO ()
run config = do
    state <- new config
    runEnv 8888 $ genericServeT (flip runReaderT state) mockchain


------------------------------------------------------------------------------
main :: IO ()
main = execParser parse >>= run
