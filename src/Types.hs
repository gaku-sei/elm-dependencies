{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Types
    ( Dependencies(direct)
    , ElmJson(dependencies)
    , Entry(name, versions)
    , NewerDependencyMap(NewerDependencyMap)
    , SearchJson
    , Version
    , findLaterVersions
    , versionParser
    )
where

import           Data.Aeson                     ( FromJSON
                                                , Value(String)
                                                , parseJSON
                                                )
import           Data.Aeson.Types               ( typeMismatch )
import           Data.List                      ( intercalate )
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Text.ParserCombinators.ReadP   ( ReadP
                                                , char
                                                , eof
                                                , manyTill
                                                , readP_to_S
                                                , satisfy
                                                )
import           Text.Read                      ( readMaybe )

-- The version record and its parser
-- Elm versions are always formatted like the following: major.minor.patch
data Version = Version { major :: Int
                       , minor :: Int
                       , patch :: Int
                       } deriving (Generic, Eq)

versionParser :: ReadP (Maybe Version)
versionParser = do
    major <- int $ char '.'
    minor <- int $ char '.'
    patch <- int eof
    pure $ Version <$> major <*> minor <*> patch
  where
    int :: ReadP a -> ReadP (Maybe Int)
    int p = readMaybe <$> (satisfy $ \c -> c >= '0' && c <= '9') `manyTill` p

instance Ord Version where
    compare v v'
        | major v > major v' = GT
        | major v < major v' = LT
        | minor v > minor v' = GT
        | minor v < minor v' = LT
        | patch v > patch v' = GT
        | patch v < patch v' = LT
        | major v == major v' && minor v == minor v' && patch v == patch v' = EQ
        | otherwise          = EQ

instance FromJSON Version where
    parseJSON (String s)
        | [(Just v, _)] <- readP_to_S versionParser $ T.unpack s = pure v
    parseJSON invalid = typeMismatch "Version" invalid

-- Isomorphic show instance
instance Show Version where
    show (Version major minor patch) =
        (show major) <> "." <> (show minor) <> "." <> (show patch)

-- The elm.json direct dependencies
data Dependencies = Dependencies { direct :: (Map.Map String Version)
                                 } deriving (Generic, Show)

instance FromJSON Dependencies

-- The elm.json root object
data ElmJson = ElmJson { dependencies :: Dependencies
                       } deriving (Generic, Show)

instance FromJSON ElmJson

-- The entry record fetched from the elm package search json
data Entry = Entry { name :: String
                   , versions :: [Version]
                   } deriving (Generic, Show)

instance FromJSON Entry

-- The root new type fetched from the elm package search json
newtype SearchJson = SearchJson [Entry] deriving (Generic, Show)

instance FromJSON SearchJson

findLaterVersions :: SearchJson -> String -> Version -> [Version]
findLaterVersions (SearchJson entries) k v = (<) v `filter` allVersions
  where
    allVersions :: [Version]
    allVersions =
        maybe [] versions $ headMaybe $ ((==) k . name) `filter` entries

    headMaybe :: [a] -> Maybe a
    headMaybe []      = Nothing
    headMaybe (x : _) = Just x

-- This new type is used solely to isolate the pretty print logic for the dependencies
-- TODO: Should be non empty list instead of list
newtype NewerDependencyMap = NewerDependencyMap (Map.Map String [Version])

instance Show NewerDependencyMap where
    show (NewerDependencyMap dependencyMap) =
        intercalate "\n" $ Map.foldrWithKey format [] dependencyMap
      where
        format :: String -> [Version] -> [String] -> [String]
        format k v = ([maybe "" (((k <> ": ") <>) . show) $ maximumMaybe v] ++)

        maximumMaybe :: (Ord a, Foldable f) => f a -> Maybe a
        maximumMaybe xs | null xs   = Nothing
                        | otherwise = Just $ maximum xs
