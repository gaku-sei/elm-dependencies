{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Map                      as Map
import           Types                          ( findLaterVersions )
import           Effects                        ( fetchSearch
                                                , getDependencies
                                                , logNewerDependencyMap
                                                )
import           System.Console.CmdArgs         ( Data
                                                , Typeable
                                                , cmdArgs
                                                , help
                                                , program
                                                , summary
                                                , typFile
                                                , (&=)
                                                )

data Args = Args {project :: String} deriving (Show, Data, Typeable)

main :: IO ()
main = do
    Args { project } <- cmdArgs args

    putStrLn "Fetching latest dependencies versions..."

    dependencyMap <- getDependencies project

    searchJson    <- fetchSearch

    logNewerDependencyMap $ Map.filter (not . null) $ Map.mapWithKey
        (findLaterVersions searchJson)
        dependencyMap
  where
    args :: Args
    args =
        Args { project = "./elm.json" &= typFile &= help "Project path" }
            &= summary "Elm dependencies v0.1.0"
            &= program "elm-dependencies"
