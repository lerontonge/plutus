{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Plutus.PAB.Run.Command
    ( ConfigCommand(..)
    , MockServerMode(..)
    ) where

import           Cardano.Node.Types (MockServerMode (..))
import qualified Data.Aeson         as JSON
import           GHC.Generics       (Generic)
import           Wallet.Types       (ContractInstanceId)

-- | A command for which a config.yaml file is required
data ConfigCommand =
    MockNode MockServerMode -- ^ Run the mock node service without starting the server
    | MockWallet -- ^ Run the mock wallet service
    | ChainIndex -- ^ Run the chain index service
    | Metadata -- ^ Run the mock meta-data service
    | ForkCommands [ConfigCommand] -- ^ Fork  a list of commands
    -- | InstallContract ContractExe -- ^ Install a contract
    | ContractState ContractInstanceId -- ^ Display the contract identified by 'ContractInstanceId'
    | ReportContractHistory ContractInstanceId -- ^ Get the history of the contract identified by 'UUID'
    -- | ReportInstalledContracts -- ^ Get installed contracts
    | ReportActiveContracts -- ^ Get active contracts
    | PABWebserver -- ^ Run the PAB webserver
    deriving stock (Show, Eq, Generic)
    deriving anyclass JSON.ToJSON
