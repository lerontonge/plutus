{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Command
    ( NoConfigCommand (..)
    ) where

import qualified Data.Aeson   as JSON
import           GHC.Generics (Generic)

-- | Commands that can be interpreted with 'runCliCommand'
data NoConfigCommand =
    Migrate { dbPath :: !FilePath } -- ^ Execute a database migration
    | PSGenerator -- ^ Generate purescript bridge code
          { outputDir :: !FilePath -- ^ Path to write generated code to
          }
    | WriteDefaultConfig -- ^ Write default logging configuration
          { outputFile :: !FilePath -- ^ Path to write configuration to
          }
    deriving stock (Show, Eq, Generic)
    deriving anyclass JSON.ToJSON
