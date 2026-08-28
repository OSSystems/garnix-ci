module Garnix.UserLogsSpec where

import Control.Lens
import Data.Aeson
import Data.ByteString.Lazy
import Garnix.BuildLogs.Types (LogLine (LogLine))
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.Types
import Garnix.UserLogs
import Test.Hspec

-- When changing assertions for `metadata` and `query` you should also
-- re-generate a valid mock response by querying OpenSearch.
--
-- For example, encode `metadata` and `query` as new line separated json in a
-- file called data, then run:
-- ```
-- curl -u opensearch -X POST --data-binary @data -H "Content-Type: application/json" "https://opensearch.garnix.io/_msearch
-- ```
mkOpenSearchReqMock :: Build -> (Value, Value) -> M ByteString
mkOpenSearchReqMock build (metadata, query) = do
  liftIO
    $ metadata
    `shouldBe` [aesonQQ|
                   {index: [
                     "garnix-build-logs-2024.05.05"
                   ]}
                 |]
  liftIO
    $ query
    `shouldBe` [aesonQQ|
                   {
                     query: { bool: { filter: [
                       { term: { "buildId.keyword": { value: #{build ^. id} } } },
                       { range: { "@timestamp": { gt: null } } }
                     ] } },
                     sort: [{ "@timestamp": { order: "asc" } }],
                     size: 42
                   }
                 |]
  pure
    $ encode
      [aesonQQ|
          {
            "took": 4,
            "responses": [
              {
                "took": 4,
                "timed_out": false,
                "_shards": {
                  "total": 2,
                  "successful": 2,
                  "skipped": 0,
                  "failed": 0
                },
                "hits": {
                  "total": {
                    "value": 2,
                    "relation": "eq"
                  },
                  "max_score": null,
                  "hits": [
                    {
                      "_index": "garnix-build-logs-2024.05.05",
                      "_id": "_RaL348B49hvkb1MGU_n",
                      "_score": null,
                      "_source": {
                        "@timestamp": "2024-05-05T00:30:00Z",
                        "branch": "main",
                        "buildId": #{build ^. id},
                        "commit": "abc123",
                        "message": "some message",
                        "package": null,
                        "phase": null,
                        "repoName": "some-repo",
                        "repoOwner": "some-owner",
                        "requestingUser": "some-owner"
                      },
                      "sort": [
                        1717442385371
                      ]
                    },
                    {
                      "_index": "garnix-build-logs-2024.05.05",
                      "_id": "GxaL348B49hvkb1MGVDn",
                      "_score": null,
                      "_source": {
                        "@timestamp": "2024-05-05T00:40:00Z",
                        "branch": "main",
                        "buildId": #{build ^. id},
                        "commit": "abc123",
                        "message": "some message 2",
                        "package": "some-package",
                        "phase": "some-phase",
                        "repoName": "some-repo",
                        "repoOwner": "some-owner",
                        "requestingUser": "some-owner"
                      },
                      "sort": [
                        1717442385383
                      ]
                    }
                  ]
                },
                "status": 200
              }
            ]
          }
        |]

portWithNoCollectorListening :: Int
portWithNoCollectorListening = 1

spec :: Spec
spec = describe "UserLogs" $ do
  describe "storeBuildLogLine"
    $ it "returns without throwing when the local collector is unreachable"
    $ runTestM
    $ do
      build <- testBuild identity
      withUnmock #storeLogLineMock
        $ local (#buildLogsReportingPort ?~ portWithNoCollectorListening)
        $ storeBuildLogLine build (LogLine (Just "some-package") Nothing "line written during a restart")

  describe "queryOpenSearch"
    $ it "parses OpenSearch responses"
    $ do
      runTestM $ do
        build <-
          testBuild
            $ (startTime .~ parseTimestamp "2024-05-05T00:00:00Z")
            . (endTime ?~ parseTimestamp "2024-05-05T01:00:00Z")
        withUnmock #queryOpenSearchMock
          $ withMock #makeOpenSearchMsearchRequestMock (mkOpenSearchReqMock build)
          $ do
            logs <- getLogLines build 42 Nothing
            liftIO
              $ logs
              `shouldBe` [ OpenSearchMessage
                             { _openSearchMessageTimestamp = parseTimestamp "2024-05-05T00:30:00Z",
                               _openSearchMessagePackage = Nothing,
                               _openSearchMessagePhase = Nothing,
                               _openSearchMessageLogMessage = "some message"
                             },
                           OpenSearchMessage
                             { _openSearchMessageTimestamp = parseTimestamp "2024-05-05T00:40:00Z",
                               _openSearchMessagePackage = Just "some-package",
                               _openSearchMessagePhase = Just "some-phase",
                               _openSearchMessageLogMessage = "some message 2"
                             }
                         ]
