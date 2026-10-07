module Garnix.DB.ModuleValues where

import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Row (Rec, (.+), (.==), type (.+), type (.==))
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude hiding (get)
import Garnix.Types

type GetRepoAndModuleValues =
  Rec
    ( "repo_user" .== Maybe GhRepoOwner
        .+ "repo_name" .== Maybe GhRepoName
        .+ "user_config" .== [ModuleValue]
        .+ "modules" .== [Module]
    )

type UpdateRepoModuleValues =
  Rec
    ( "repo_user" .== Maybe GhRepoOwner
        .+ "repo_name" .== Maybe GhRepoName
        .+ "user_config" .== [ModuleValue]
    )

type ModuleValue =
  Rec
    ( "module_name" .== Text
        .+ "git_commit" .== Maybe CommitHash
        .+ "values" .== ModuleConfig
    )

type Module =
  Rec
    ( "name" .== Text
        .+ "repo_user" .== GhRepoOwner
        .+ "repo_name" .== GhRepoName
        .+ "git_commit" .== CommitHash
        .+ "schema" .== JSON.Value
        .+ "description" .== Maybe Text
    )

data ModuleConfig = ModuleConfig NixIdentifier NixValue
  deriving stock (Generic)

instance FromJSON ModuleConfig where
  parseJSON = JSON.withObject "ModuleConfig" $ \o -> do
    tag <- o .: "tag"
    when (tag /= "set") $ fail $ "expected 'set' tag at the top level, got: " <> tag
    obj <- o .: "value"
    case JSON.toList obj of
      [(identifier, value)] -> do
        nixValue <- parseJSON value
        pure $ ModuleConfig (NixIdentifier $ JSON.toText identifier) nixValue
      _ -> fail $ "expected a single value at the top level, got: " <> cs (show obj)

instance ToJSON ModuleConfig where
  toJSON (ModuleConfig identifier value) =
    JSON.object
      [ "tag" .= ("set" :: Text),
        "value"
          .= JSON.object
            [ JSON.fromText (getNixIdentifier identifier) .= value
            ]
      ]

instance PGColumn "json" ModuleConfig where
  pgDecode _ =
    either (\message -> error $ "Cannot decode ModuleConfig json value: " <> cs message) identity
      . JSON.eitherDecode @ModuleConfig
      . cs

instance PGParameter "json" ModuleConfig where
  pgEncode _ = cs . JSON.encode

newtype NixIdentifier = NixIdentifier {getNixIdentifier :: Text}
  deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (FromJSONKey, ToJSONKey)

data GithubRepository = GithubRepository
  { repoUser :: GhRepoOwner,
    repoName :: GhRepoName
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving (FromJSON, ToJSON)

data EncryptedSecret = EncryptedSecret
  { encryptedFor :: GithubRepository,
    encryptedValue :: Text
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving (FromJSON, ToJSON)

data NixValue
  = Secret EncryptedSecret
  | NixString Text
  | NixPath Text
  | NixRaw Text
  | NixBool Bool
  | NixInt Int
  | NixNull
  | NixList [NixValue]
  | NixSet (Map NixIdentifier NixValue)
  deriving stock (Eq, Ord, Generic, Show)

instance FromJSON NixValue where
  parseJSON = JSON.withObject "NixValue" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "encryptedSecret" -> Secret <$> o .: "value"
      "string" -> NixString <$> o .: "value"
      "path" -> NixPath <$> o .: "value"
      "raw" -> NixRaw <$> o .: "value"
      "bool" -> NixBool <$> o .: "value"
      "int" -> NixInt <$> o .: "value"
      "null" -> pure NixNull
      "list" -> NixList <$> o .: "value"
      "set" -> NixSet <$> o .: "value"
      other -> fail $ "unexpected tag found: " <> cs (show other)

instance ToJSON NixValue where
  toJSON = \case
    Secret s -> withTag "encryptedSecret" s
    NixString s -> withTag "string" s
    NixPath p -> withTag "path" p
    NixRaw r -> withTag "raw" r
    NixBool b -> withTag "bool" b
    NixInt n -> withTag "int" n
    NixNull -> JSON.object ["tag" .= ("null" :: Text)]
    NixList l -> withTag "list" l
    NixSet s -> withTag "set" s
    where
      withTag :: (ToJSON a) => Text -> a -> JSON.Value
      withTag tag value =
        JSON.object
          [ "tag" .= tag,
            "value" .= value
          ]

get :: ForgeLogin -> M (Maybe GetRepoAndModuleValues)
get (ForgeLogin forge' ghLogin) =
  DB.pgQuery
    [pgSQL|
        SELECT id, repo_user, repo_name
        FROM module_user_repo
        WHERE forge = ${forge'}
          AND github_login = ${ghLogin}
      |]
    >>= \case
      [] -> pure Nothing
      [(id, repo_user, repo_name)] -> do
        userConfig <- getConfig id
        modules <- getModules (fst <$> userConfig)
        pure
          $ Just
            ( #repo_user .== repo_user
                .+ #repo_name .== repo_name
                .+ #user_config .== (snd <$> userConfig)
                .+ #modules .== modules
            )
      _ -> throw $ OtherError "Got more than 1 value for ModuleValues.get"
  where
    getConfig :: Int64 -> M [(Int64, ModuleValue)]
    getConfig id =
      DB.pgQuery
        [pgSQL|
          SELECT modules.id, modules.name, modules.git_commit, module_values.values
          FROM module_user_repo
            JOIN module_values on module_user_repo.id = module_user_repo_id
            JOIN modules on modules.id = module_id
          WHERE module_user_repo_id = ${id}
        |]
        <&> fmap
          ( \(id, name, git_commit, values) ->
              ( id,
                #module_name .== name
                  .+ #values .== values
                  .+ #git_commit .== Just git_commit
              )
          )

    getModules :: [Int64] -> M [Module]
    getModules ids =
      DB.pgQuery
        [pgSQL|
          SELECT repo_user, repo_name, git_commit, schema, name, description
          FROM modules
          WHERE 
            id = ANY(${ids})
            OR 
              ( enabled = true 
                AND name NOT IN (SELECT name from modules where id = ANY(${ids}))
              ) 
              
        |]
        <&> fmap
          ( \(repo_user, repo_name, git_commit, schema, name, description) ->
              #name .== name
                .+ #repo_user .== repo_user
                .+ #repo_name .== repo_name
                .+ #git_commit .== CommitHash git_commit
                .+ #schema .== schema
                .+ #description .== description
          )

update :: ForgeLogin -> UpdateRepoModuleValues -> M ()
update (ForgeLogin forge' ghLogin) row = do
  let repo_user = row ^. #repo_user
      repo_name = row ^. #repo_name
      user_config = row ^. #user_config
  void
    $ DB.pgExec
      [pgSQL|
        INSERT INTO module_user_repo
          (forge, github_login, repo_user, repo_name)
          VALUES (${forge'}, ${ghLogin}, ${repo_user}, ${repo_name})
        ON CONFLICT (forge, github_login) DO
          UPDATE
            SET repo_user = ${repo_user},
                repo_name = ${repo_name}
      |]
  DB.pgTransaction $ do
    removeAllValues
    traverse_ addValue user_config
  where
    removeAllValues :: M ()
    removeAllValues = do
      void
        $ DB.pgExec
          [pgSQL|
            DELETE FROM module_values
              WHERE module_user_repo_id = (SELECT id FROM module_user_repo WHERE forge = ${forge'} AND github_login = ${ghLogin})
          |]

    addValue :: ModuleValue -> M ()
    addValue value =
      let values = value ^. #values
          module_name = value ^. #module_name
          git_commit = value ^. #git_commit
       in case git_commit of
            Just commit ->
              void
                $ DB.pgExec
                  [pgSQL|
                      INSERT INTO module_values
                        (module_user_repo_id, module_id, values)
                        VALUES (
                          (SELECT id FROM module_user_repo WHERE forge = ${forge'} AND github_login = ${ghLogin}),
                          (SELECT id FROM modules WHERE name = ${module_name} and git_commit = ${commit}),
                          ${values}
                        )
                    |]
            Nothing ->
              void
                $ DB.pgExec
                  [pgSQL|
                      INSERT INTO module_values
                        (module_user_repo_id, module_id, values)
                        VALUES (
                          (SELECT id FROM module_user_repo WHERE forge = ${forge'} AND github_login = ${ghLogin}),
                          (SELECT id FROM modules WHERE name = ${module_name} and enabled = true),
                          ${values}
                        )
                    |]

delete :: ForgeLogin -> M ()
delete (ForgeLogin forge' ghLogin) = do
  void
    $ DB.pgExec
      [pgSQL|
        DELETE FROM module_values
          WHERE module_user_repo_id =
            (SELECT id
              FROM module_user_repo
              WHERE forge = ${forge'}
                AND github_login = ${ghLogin})
      |]
  void
    $ DB.pgExec
      [pgSQL|
        DELETE FROM module_user_repo
          WHERE forge = ${forge'}
            AND github_login = ${ghLogin}
      |]

getAvailableModules :: M [Module]
getAvailableModules =
  DB.pgQuery
    [pgSQL|
      SELECT repo_user, repo_name, git_commit, schema, name, description
      FROM modules
      WHERE enabled = true
    |]
    <&> fmap
      ( \(repo_user, repo_name, git_commit, schema, name, description) ->
          #name .== name
            .+ #repo_user .== repo_user
            .+ #repo_name .== repo_name
            .+ #git_commit .== CommitHash git_commit
            .+ #schema .== schema
            .+ #description .== description
      )

insertLatestVersion :: Module -> M ()
insertLatestVersion mod =
  DB.pgTransaction $ do
    let name = mod ^. #name
        repo_user = mod ^. #repo_user
        repo_name = mod ^. #repo_name
        git_commit = mod ^. #git_commit
        description = mod ^. #description
        schema = mod ^. #schema
    void
      $ DB.pgExec
        [pgSQL|
          UPDATE modules
            SET enabled = false
            WHERE name = ${name}
        |]
    result <-
      DB.pgExec
        [pgSQL|
          INSERT INTO modules
            (repo_user, repo_name, git_commit, enabled, name, description, schema)
            VALUES
            (${repo_user}, ${repo_name}, ${git_commit}, true, ${name}, ${description}, ${schema})
        |]
    when (result /= 1) $ throw $ OtherError "Could not insert module to DB."
