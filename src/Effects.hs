module Effects
    ( fetchSearch
    , getDependencies
    , logNewerDependencyMap
    )
where

import           Control.Lens                   ( (<&>)
                                                , (^.)
                                                )
import           Data.Aeson                     ( decode )
import qualified Data.ByteString.Lazy          as B
import qualified Data.Map                      as Map
import           Network.Wreq                   ( get
                                                , responseBody
                                                )
import           Types                          ( Dependencies(direct)
                                                , ElmJson(dependencies)
                                                , NewerDependencyMap
                                                    ( NewerDependencyMap
                                                    )
                                                , SearchJson
                                                , Version
                                                )

logNewerDependencyMap :: Map.Map String [Version] -> IO ()
logNewerDependencyMap m
    | null m    = putStrLn "Your dependencies are all up to date"
    | otherwise = putStrLn $ show $ NewerDependencyMap m

fetchSearch :: IO SearchJson
fetchSearch = do
    let impossibleToFetch =
            "An error occured while fetching the dependencies db"
    r <- get "https://package.elm-lang.org/search.json"
    pure $ maybe (error impossibleToFetch) id $ decode $ r ^. responseBody

getDependencies :: FilePath -> IO (Map.Map String Version)
getDependencies path =
    let invalidElmJsonMessage =
                "The provided elm.json file doesn't seem to be valid"
    in  decode
            <$> B.readFile path
            <&> maybe (error invalidElmJsonMessage) dependencies
            <&> direct
