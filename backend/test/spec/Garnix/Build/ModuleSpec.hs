module Garnix.Build.ModuleSpec where

import Control.Lens
import Data.Aeson (throwDecode')
import Data.Aeson.Lens
import Data.Aeson.Types qualified as Aeson
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Text
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.Build.Checkout qualified as Checkout
import Garnix.Build.Module
import Garnix.DB qualified as DB
import Garnix.DB.ModuleValues qualified as ModuleValues
import Garnix.Prelude
import Garnix.TestHelpers (defaultCommitInfo, shouldMatchRegexp, truncateDBM)
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.Types (Branch (..), Error (..), ErrorWithContext (..), err)
import Test.Hspec

spec :: Spec
spec = inM $ do
  describe "NixValue" $ do
    let cases =
          [ ([i| { "tag": "encryptedSecret", "value": { "encryptedValue": "<SOME AGE ENCRYPTED STRING>", "encryptedFor": { "repoUser": "some-owner", "repoName": "some-repo" } } } |], "\"<SOME AGE ENCRYPTED STRING>\""),
            ([i| { "tag": "string", "value": "foo"} |], "\"foo\""),
            ([i| { "tag": "path", "value": "./."} |], "./."),
            ([i| { "tag": "raw", "value": "pkgs.hello"} |], "pkgs.hello"),
            ([i| { "tag": "bool", "value": true} |], "true"),
            ([i| { "tag": "int", "value": 42} |], "42"),
            ([i| { "tag": "null" } |], "null"),
            ( [i| { "tag": "set", "value": { "foo": { "tag": "string", "value": "bar" } } }|],
              "{\n        foo = \"bar\";\n      }"
            ),
            ([i| { "tag": "list", "value": [{ "tag": "int", "value": 42}]} |], "[ 42 ]")
          ]

    forM_ cases $ \(jsonInput, expectedNix) -> do
      it ("understands " <> jsonInput ^?! key "tag" . _String . to cs) $ do
        let test nixValue expected = do
              parsed <- throwDecode' $ cs nixValue
              strip (_moduleConfig (ModuleValues.ModuleConfig (ModuleValues.NixIdentifier "foo") parsed))
                `shouldBeM` ("foo = " <> expected <> ";")
        test jsonInput expectedNix

  describe "generateFlakeNix" $ beforeM_ truncateDBM $ do
    it "generates flake.nix file correctly" $ do
      1 <-
        DB.pgExec
          [pgSQL|
            INSERT INTO modules
              (repo_user, repo_name, git_commit, enabled, name, schema)
              VALUES
              ('garnix-io', 'test-module', 'test-module-commit', true, 'testModule', '"test schema"')
          |]
      let moduleConfig =
            [aesonQQ|
              {
                "repo_user": "",
                "repo_name": "",
                "user_config": [{
                  "module_name": "testModule",
                  "git_commit": "",
                  "values": {
                    "tag": "set",
                    "value": {
                      "testModule": {
                        "tag": "set",
                        "value": {
                          "backend": {
                            "tag": "set",
                            "value": {
                              "devShellPackages": {
                                "tag": "list",
                                "value": [
                                  {
                                    "tag": "raw",
                                    "value": "pkgs.foo"
                                  },
                                  {
                                    "tag": "raw",
                                    "value": "pkgs.bar"
                                  }
                                ]
                              },
                              "serverCommand": {
                                "tag": "string",
                                "value": "runBackend"
                              },
                              "src": {
                                "tag": "path",
                                "value": "./src"
                              },
                              "some-secret": {
                                "tag": "encryptedSecret",
                                "value": {
                                  "encryptedFor": {
                                    "repoUser": "some-user",
                                    "repoName": "some-repo"
                                  },
                                  "encryptedValue": "<SOME AGE ENCRYPTED STRING>"
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }],
                "modules": [{
                  "name": "testModule",
                  "repo_user": "garnix-io",
                  "repo_name": "test-module",
                  "git_commit": "",
                  "schema": {},
                  "description": ""
                }]
              }
            |]
      contents <- generateFlakeNix (Branch "test") $ either (error . cs) identity $ Aeson.parseEither Aeson.parseJSON moduleConfig
      let expected =
            cs
              $ unindent
                [i|
                  {
                    inputs = {
                      garnix-lib.url = "github:garnix-io/garnix-lib";
                      testModule.url = "github:garnix-io/test-module";
                    };

                    nixConfig = {
                      extra-substituters = [ "https://cache.garnix.io" ];
                      extra-trusted-public-keys = [ "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=" ];
                    };

                    outputs = inputs: inputs.garnix-lib.lib.mkModules {
                      modules = [
                        inputs.testModule.garnixModules.default
                      ];

                      config = { pkgs, ... }: {
                        testModule = {
                          backend = {
                            devShellPackages = [ pkgs.foo pkgs.bar ];
                            serverCommand = "runBackend";
                            some-secret = "<SOME AGE ENCRYPTED STRING>";
                            src = ./src;
                          };
                        };

                        garnix.deployBranch = "test";
                      };
                    };
                  }
                |]
      contents `shouldBeM` expected

    it "handles multi-modules correctly" $ do
      2 <-
        DB.pgExec
          [pgSQL|
            INSERT INTO modules
              (repo_user, repo_name, git_commit, enabled, name, schema)
              VALUES
              ('garnix-io1', 'test-module1', 'test-module-commit1', true, 'testModule1', '"test schema"'),
              ('garnix-io2', 'test-module2', 'test-module-commit2', true, 'testModule2', '"test schema"')
          |]
      let moduleConfig =
            [aesonQQ|
              {
                "repo_user": "",
                "repo_name": "",
                "user_config": [{
                "module_name": "testModule1",
                "git_commit": "",
                "values": {
                  "tag": "set",
                  "value": {
                    "testModule1": {
                      "tag": "set",
                      "value": {
                        "foo": {
                          "tag": "string",
                          "value": "foo"
                        }
                      }
                    }
                  }
                }
              }, {
                "module_name": "testModule2",
                "git_commit": "",
                "values": {
                  "tag": "set",
                  "value": {
                    "testModule2": {
                      "tag": "set",
                      "value": {
                        "bar": {
                          "tag": "string",
                          "value": "bar"
                        }
                      }
                    }
                  }
                }
              }],
                "modules": [{
                  "name": "testModule1",
                  "repo_user": "garnix-io1",
                  "repo_name": "test-module1",
                  "git_commit": "",
                  "schema": {},
                  "description": ""
                },{
                  "name": "testModule2",
                  "repo_user": "garnix-io2",
                  "repo_name": "test-module2",
                  "git_commit": "",
                  "schema": {},
                  "description": ""
                }
                ]
              }
            |]
      contents <- generateFlakeNix (Branch "test") $ either (error . cs) identity $ Aeson.parseEither Aeson.parseJSON moduleConfig
      let expected =
            cs
              $ unindent
                [i|
                  {
                    inputs = {
                      garnix-lib.url = "github:garnix-io/garnix-lib";
                      testModule1.url = "github:garnix-io1/test-module1";
                      testModule2.url = "github:garnix-io2/test-module2";
                    };

                    nixConfig = {
                      extra-substituters = [ "https://cache.garnix.io" ];
                      extra-trusted-public-keys = [ "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=" ];
                    };

                    outputs = inputs: inputs.garnix-lib.lib.mkModules {
                      modules = [
                        inputs.testModule1.garnixModules.default
                        inputs.testModule2.garnixModules.default
                      ];

                      config = { pkgs, ... }: {
                        testModule1 = {
                          foo = "foo";
                        };
                        testModule2 = {
                          bar = "bar";
                        };

                        garnix.deployBranch = "test";
                      };
                    };
                  }
                |]
      contents `shouldBeM` expected

    it "handles using older versions of modules correctly" $ GH.withFakeGithubInterface $ \ghState -> do
      2 <-
        DB.pgExec
          [pgSQL|
            INSERT INTO modules
              (repo_user, repo_name, git_commit, enabled, name, schema)
              VALUES
              ('garnix-io', 'test-module', 'test-module-newer-commit', true , 'testModule', '"test schema"'),
              ('garnix-io', 'test-module', 'test-module-older-commit', false, 'testModule', '"test schema"')
          |]
      let moduleConfig =
            [aesonQQ|
              {
                "repo_user": "",
                "repo_name": "",
                "user_config": [{
                  "module_name": "testModule",
                  "git_commit": "test-module-older-commit",
                  "values": {
                    "tag": "set",
                    "value": {
                      "testModule": {
                        "tag": "set",
                        "value": {
                          "backend": {
                            "tag": "set",
                            "value": {
                              "devShellPackages": {
                                "tag": "list",
                                "value": [
                                  {
                                    "tag": "raw",
                                    "value": "pkgs.foo"
                                  },
                                  {
                                    "tag": "raw",
                                    "value": "pkgs.bar"
                                  }
                                ]
                              },
                              "serverCommand": {
                                "tag": "string",
                                "value": "runBackend"
                              },
                              "src": {
                                "tag": "path",
                                "value": "./src"
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }],
                "modules": [{
                  "name": "testModule",
                  "repo_user": "garnix-io",
                  "repo_name": "test-module",
                  "git_commit": "test-module-older-commit",
                  "schema": {},
                  "description": ""
                }]
              }
            |]
      let config = either (error . cs) identity $ Aeson.parseEither Aeson.parseJSON moduleConfig
      let remote = remoteWithFlake (Branch "test") config Checkout.remoteWithConfig
      GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo (const (pure ())) $ \commitInfo -> do
        result <- try $ Checkout.runWithCheckout remote commitInfo (const (pure ()))
        case result of
          Left ErrorWithContext {err = RunProcessError {stdErr}} -> stdErr `shouldMatchRegexp` "error: unable to download 'https://api.github.com/repos/garnix-io/test-module/commits/test-module-older-commit'"
          Left ErrorWithContext {err} -> liftIO $ expectationFailure $ "Expected RunProcessError but got " <> cs (Garnix.Prelude.show err)
          _ -> liftIO $ expectationFailure $ "Expected Left RunProcessError but got " <> cs (Garnix.Prelude.show result)

    it "correctly escapes values" $ do
      1 <-
        DB.pgExec
          [pgSQL|
            INSERT INTO modules
              (repo_user, repo_name, git_commit, enabled, name, schema)
              VALUES
              ('garnix-io', 'test-module', 'test-module-commit', true, 'testModule', '"test schema"')
          |]
      let moduleConfig =
            [aesonQQ|
              {
                "repo_user": "",
                "repo_name": "",
                "user_config": [{
                  "module_name": "testModule",
                  "git_commit": "test-module-older-commit",
                  "values": {
                    "tag": "set",
                    "value": {
                      "testModule": {
                        "tag": "set",
                        "value": {
                          "secretValue": {
                            "tag": "encryptedSecret",
                            "value": {
                              "encryptedFor": {
                                "repoUser": "",
                                "repoName": ""
                              },
                              "encryptedValue": "quote: \", backslash: \\"
                            }
                          },
                          "stringValue": {
                            "tag": "string",
                            "value": "quote: \", backslash: \\"
                          }
                        }
                      }
                    }
                  }
                }],
                "modules": [{
                  "name": "testModule",
                  "repo_user": "garnix-io",
                  "repo_name": "test-module",
                  "git_commit": "test-module-older-commit",
                  "schema": {},
                  "description": ""
                }]
              }
            |]
      contents <- generateFlakeNix (Branch "test") $ either (error . cs) identity $ Aeson.parseEither Aeson.parseJSON moduleConfig
      let expected =
            cs
              $ unindent
                [i|
                  {
                    inputs = {
                      garnix-lib.url = "github:garnix-io/garnix-lib";
                      testModule.url = "github:garnix-io/test-module";
                    };

                    nixConfig = {
                      extra-substituters = [ "https://cache.garnix.io" ];
                      extra-trusted-public-keys = [ "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=" ];
                    };

                    outputs = inputs: inputs.garnix-lib.lib.mkModules {
                      modules = [
                        inputs.testModule.garnixModules.default
                      ];

                      config = { pkgs, ... }: {
                        testModule = {
                          secretValue = #{"\"quote: \\\", backslash: \\\\\"" :: Text};
                          stringValue = #{"\"quote: \\\", backslash: \\\\\"" :: Text};
                        };

                        garnix.deployBranch = "test";
                      };
                    };
                  }
                |]
      contents `shouldBeM` expected
