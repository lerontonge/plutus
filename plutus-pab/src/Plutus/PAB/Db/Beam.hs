{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-

Interface to beam ecosystem used by the PAB to store contracts.

-}
module Plutus.PAB.Db.Beam
  where

import           Cardano.BM.Trace                           (Trace)
import           Control.Monad.Freer                        (Eff, interpret, runM, subsume)
import           Control.Monad.Freer.Delay                  (DelayEffect, handleDelayEffect)
import           Control.Monad.Freer.Error                  (runError)
import qualified Control.Monad.Freer.Extras.Modify          as Modify
import           Control.Monad.Freer.Reader                 (runReader)
import           Database.SQLite.Simple                     (Connection)
import           Plutus.PAB.Db.Beam.ContractDefinitionStore (handleContractDefinitionStore)
import           Plutus.PAB.Db.Beam.ContractStore           (handleContractStore)
import           Plutus.PAB.Effects.Contract                (ContractDefinitionStore, ContractStore)
import           Plutus.PAB.Effects.Contract.Builtin        (Builtin)
import           Plutus.PAB.Effects.DbStore
import           Plutus.PAB.Monitoring.PABLogMsg            (PABLogMsg)
import           Plutus.PAB.Types                           (PABError)

-- | Run the ContractStore and ContractDefinitionStore effects on the
--   SQLite database.
runBeamStoreAction ::
    forall a b.
    (Show a, Read a)
    => Connection
    -> Trace IO (PABLogMsg (Builtin a))
    -> Eff '[ContractDefinitionStore (Builtin a), ContractStore (Builtin a), DelayEffect, IO] b
    -> IO (Either PABError b)
runBeamStoreAction connection trace =
    runM
    . runError
    . runReader connection
    . interpret (handleDbStore trace)
    . subsume @IO
    . handleDelayEffect
    . interpret handleContractStore
    . interpret handleContractDefinitionStore
    . Modify.raiseEnd
