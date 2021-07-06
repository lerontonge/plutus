{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module CommandParser (parseOptions, AppOpts(..)) where

import           Cardano.BM.Data.Severity (Severity (..))
import           Command
import           Options.Applicative      (CommandFields, Mod, Parser, argument, command, customExecParser,
                                           disambiguate, flag, fullDesc, help, helper, idm, info, long, metavar, option,
                                           prefs, progDesc, short, showHelpOnEmpty, showHelpOnError, str, subparser,
                                           value)
import           Plutus.PAB.App           (StorageBackend (..))

data AppOpts = AppOpts { minLogLevel    :: Maybe Severity
                       , logConfigPath  :: Maybe FilePath
                       , runEkgServer   :: Bool
                       , storageBackend :: StorageBackend
                       , cmd            :: NoConfigCommand
                       }

parseOptions :: IO AppOpts
parseOptions = customExecParser
            (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
            (info (helper <*> commandLineParser) idm)

logLevelFlag :: Parser (Maybe Severity)
logLevelFlag =
    flag
        Nothing
        (Just Debug)
        (short 'v' <> long "verbose" <> help "Enable debugging output.")

ekgFlag :: Parser Bool
ekgFlag =
    flag
        False
        True
        (short 'e' <> long "ekg" <> help "Enable the EKG server")

inMemoryFlag :: Parser StorageBackend
inMemoryFlag =
    flag
        BeamSqliteBackend
        InMemoryBackend
        (short 'm' <> long "memory" <> help "Use the memory-backed backend. If false, the beam backend is used.")

commandLineParser :: Parser AppOpts
commandLineParser =
        AppOpts <$> logLevelFlag
                <*> logConfigFileParser
                <*> ekgFlag
                <*> inMemoryFlag
                <*> commandParser

logConfigFileParser :: Parser (Maybe FilePath)
logConfigFileParser =
    option
        (Just <$> str)
        (long "log-config" <>
         metavar "LOG_CONFIG_FILE" <>
         help "Logging config file location." <> value Nothing)

commandParser :: Parser NoConfigCommand
commandParser =
    subparser $
    mconcat
        [ migrationParser
        , psGeneratorCommandParser
        , defaultConfigParser
        ]

defaultConfigParser :: Mod CommandFields NoConfigCommand
defaultConfigParser =
    command "default-logging-config" $
    flip info (fullDesc <> progDesc "Write the default logging configuration YAML to a file") $ do
        outputFile <-
            argument
                str
                (metavar "OUTPUT_FILE" <>
                 help "Output file to write logging config YAML to.")
        pure $ WriteDefaultConfig {outputFile}

psGeneratorCommandParser :: Mod CommandFields NoConfigCommand
psGeneratorCommandParser =
    command "psgenerator" $
    flip info (fullDesc <> progDesc "Generate the frontend's PureScript files.") $ do
        outputDir <-
            argument
                str
                (metavar "OUTPUT_DIR" <>
                 help "Output directory to write PureScript files to.")
        pure $ PSGenerator {outputDir}

migrationParser :: Mod CommandFields NoConfigCommand
migrationParser =
    command "migrate" $
    flip info (fullDesc <> progDesc "Update the database with the latest schema.") $ do
        dbPath <-
            argument
                str
                (metavar "DATABASE" <>
                 help "The sqlite database file.")
        -- TODO: This will need to be 'WithConfig'.
        pure $ Migrate{dbPath}
