module Docs.Search.PackageIndex where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Search.Trie (Trie)
import Data.Search.Trie as Trie
import Data.Tuple as Tuple
import Docs.Search.Config as Config
import Docs.Search.Extra (stringToList)
import Docs.Search.JsonCodec as JsonCodec
import Docs.Search.Loader as Loader
import Docs.Search.Score (Scores, getPackageScoreForPackageName)
import Docs.Search.Types (PackageScore)
import Docs.Search.Types as Package
import Effect.Aff (Aff)
import JSON (JSON)
import Unsafe.Coerce (unsafeCoerce)
import Web.Bower.PackageMeta (PackageMeta(..), PackageName)
import Web.Bower.PackageMeta as Bower

type PackageResult =
  { name :: PackageName
  , description :: Maybe String
  , score :: PackageScore
  , dependencies :: Array PackageName
  , repository :: Maybe String
  }

packageResultCodec :: CJ.Codec PackageResult
packageResultCodec = CJ.named "PackageResult" $
  CJ.Record.object
    { name: Package.packageNameCodec
    , description: CJ.Record.optional CJ.string
    , score: Package.packageScoreCodec
    , dependencies: CJ.array Package.packageNameCodec
    , repository: CJ.Record.optional CJ.string
    }

type PackageIndex = Trie Char PackageResult

type PackageInfo = Array PackageResult

mkPackageInfo :: Scores -> Array PackageMeta -> PackageInfo
mkPackageInfo packageScores pms =
  Array.fromFoldable
    $ Map.values
    $ Array.foldr insert Map.empty pms

  where
  insert
    :: PackageMeta
    -> Map PackageName PackageResult
    -> Map PackageName PackageResult
  insert
    ( PackageMeta
        { name
        , description
        , dependencies
        , repository
        }
    ) =
    Map.insert
      name
      { name
      , description
      , score: getPackageScoreForPackageName packageScores name
      , dependencies: dependencies <#> Tuple.fst
      , repository: repository <#> unwrap >>> (_.url)
      }

mkScoresFromPackageIndex :: PackageIndex -> Scores
mkScoresFromPackageIndex =
  Trie.values >>> Array.fromFoldable >>>
    Array.foldr (\{ name, score } -> Map.insert name score) Map.empty

loadPackageIndex :: Aff PackageIndex
loadPackageIndex =
  mkPackageIndex <$> Loader.load packageInfoCodec Config.packageInfoItem Config.packageInfoLoadPath
  where
  packageInfoCodec :: CJ.Codec PackageInfo
  packageInfoCodec = CJ.array packageResultCodec

mkPackageIndex :: PackageInfo -> PackageIndex
mkPackageIndex =
  Array.foldr
    (\package -> Trie.insert (stringToList $ unwrap package.name) package)
    mempty

queryPackageIndex
  :: forall m
   . Monad m
  => PackageIndex
  -> String
  -> m
       { index :: PackageIndex
       , results :: Array PackageResult
       }
queryPackageIndex index query =
  pure
    { index
    , results: Array.fromFoldable $ Trie.queryValues (stringToList query) index
    }

packageMetaCodec :: CJ.Codec PackageMeta
packageMetaCodec = Codec.codec' decode encode
  where
  decode = JsonCodec.fromUni Bower.toPackageMeta
  encode = Bower.fromPackageMeta >>> (unsafeCoerce :: Json -> JSON)
