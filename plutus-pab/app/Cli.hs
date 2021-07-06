{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Cli (runNoConfigCommand) where

-----------------------------------------------------------------------------------------------------------------------
-- Command interpretation
-----------------------------------------------------------------------------------------------------------------------

import           Command

import           Cardano.BM.Configuration              (Configuration)
import qualified Cardano.BM.Configuration.Model        as CM
import           Cardano.BM.Data.Trace                 (Trace)
import qualified Cardano.ChainIndex.Server             as ChainIndex
import qualified Cardano.Metadata.Server               as Metadata
import qualified Cardano.Node.Server                   as NodeServer
import qualified Cardano.Wallet.Server                 as WalletServer
import           Cardano.Wallet.Types
import           Control.Concurrent                    (takeMVar)
import           Control.Concurrent.Async              (Async, async, waitAny)
import           Control.Concurrent.Availability       (Availability, starting)
import           Control.Monad                         (void)
import           Control.Monad.Freer                   (Eff, Member, interpret)
import           Control.Monad.Freer.Delay             (DelayEffect, delayThread)
import           Control.Monad.Freer.Extras.Log        (logInfo)
import           Control.Monad.IO.Class                (liftIO)
import           Data.Foldable                         (traverse_)
import qualified Data.Map                              as Map
import           Data.Proxy                            (Proxy (..))
import qualified Data.Set                              as Set
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc             (Pretty (..), defaultLayoutOptions, layoutPretty, pretty)
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Time.Units                       (Second)
import qualified Plutus.PAB.Effects.Contract           as Contract

import           Cardano.Node.Types                    (MockServerConfig (..))
import qualified PSGenerator
import           Plutus.Contract.Resumable             (responses)
import           Plutus.Contract.State                 (State (..))
import qualified Plutus.Contract.State                 as State
import qualified Plutus.PAB.App                        as App
import qualified Plutus.PAB.Core                       as Core
import qualified Plutus.PAB.Db.Beam                    as Beam
import           Plutus.PAB.Effects.Contract.Builtin   (Builtin)
import qualified Plutus.PAB.Monitoring.Monitoring      as LM
import           Plutus.PAB.Types                      (Config (..), DbConfig (..), chainIndexConfig,
                                                        metadataServerConfig, nodeServerConfig, walletServerConfig)
import qualified Plutus.PAB.Webserver.Server           as PABServer
import           Plutus.PAB.Webserver.Types            (ContractActivationArgs (..))

runNoConfigCommand ::
    Trace IO (LM.AppMsg (Builtin a))  -- ^ PAB Tracer logging instance
    -> NoConfigCommand
    -> IO ()
runNoConfigCommand trace = \case

    -- Run database migration
    Migrate{dbPath} ->
        let conf = DbConfig{dbConfigPoolSize=10, dbConfigFile=Text.pack dbPath} in
        App.migrate (LM.convertLog LM.PABMsg trace) conf

    -- Generate PureScript bridge code
    PSGenerator {outputDir} -> PSGenerator.generate outputDir

    -- Get default logging configuration
    WriteDefaultConfig{outputFile} -> LM.defaultConfig >>= flip CM.exportConfiguration outputFile
