\set ON_ERROR_STOP on

DELETE FROM builds;

-- users

TRUNCATE users CASCADE;
INSERT INTO users
  (github_login, email, subscription_type)
  VALUES
  ('dev-user', 'foo@example.com', 'free');
SELECT * FROM users;

-- builds

INSERT INTO builds
  (id, repo_user, repo_name, git_commit, package, package_type, req_user, repo_is_public, start_time, end_time, status, branch)
  OVERRIDING SYSTEM VALUE
  VALUES
  (424242, 'garnix-io', 'test-repo', 'aaaaaa', 'my-cool-server', 'package', 'dev-user', true, NOW() - interval '5 minutes', NOW(), 'success', 'main');
SELECT * FROM builds;

-- commits

TRUNCATE commits;
INSERT INTO commits
  (repo_user, repo_name, git_commit, status, meta_check)
  VALUES
  ('garnix-io', 'test-repo', 'aaaaaa', 'evaluated', 'success');
SELECT * FROM commits;

-- runs

TRUNCATE runs;
INSERT INTO runs
  (id, name, repo_user, repo_name, git_commit, req_user, start_time, end_time, status, branch)
  VALUES
  (123456, 'test run', 'garnix-io', 'test-repo', 'aaaaaa', 'dev-user', NOW() - interval '5 minutes', NOW(), 'success', 'main');
SELECT * FROM runs;

-- modules

TRUNCATE modules CASCADE;

INSERT INTO modules
  (repo_user, repo_name, git_commit, enabled, name, description, schema)
  VALUES
  ('garnix-io', 'haskell-module', '306663ad4e11d5bf0a5c2d11fd3170e83404c68b',
   true, 'Haskell', 'A garnix module for projects using Haskell.

Add dependencies, pick the GHC version, and optionally deploy a web server.

[Documentation](https://garnix.io/docs/modules/haskell) - [Source](https://github.com/garnix-io/haskell-module).
', '{"description":"A garnix module for projects using Haskell.\n\nAdd dependencies, pick the GHC version, and optionally deploy a web server.\n\n[Documentation](https://garnix.io/docs/modules/haskell) - [Source](https://github.com/garnix-io/haskell-module).\n","typ":{"fields":{"haskell":{"description":"An attrset of Haskell projects to generate.","typ":{"fieldType":{"fields":{"buildDependencies":{"default":{"tag":"list","value":[]},"description":"A list of additional dependencies required to build this package. They are made available in the devshell, and at build time.\n\n(It''s not necessary to include library dependencies manually, these will be included automatically.)\n","typ":{"elementType":{"tag":"package"},"tag":"listOf"}},"devTools":{"default":{"tag":"list","value":[]},"description":"A list of packages make available in the devshell for this project (and `default` devshell). This is useful for things like LSPs, formatters, etc.","name":"development tools","typ":{"elementType":{"tag":"package"},"tag":"listOf"}},"ghcVersion":{"default":{"tag":"string","value":"9.8"},"description":"The major GHC version to use.","name":"GHC version","typ":{"tag":"enum","variants":["9.10","9.8","9.6","9.4","9.2","9.0"]}},"runtimeDependencies":{"default":{"tag":"list","value":[]},"description":"A list of dependencies required at runtime. They are made available in the devshell, at build time, and are available on the server at runtime.","typ":{"elementType":{"tag":"package"},"tag":"listOf"}},"src":{"description":"A path to the directory containing your cabal or hpack (`package.yaml`) file.","example":"./.","name":"source directory","typ":{"tag":"path"}},"webServer":{"default":{"tag":"null"},"description":"Whether to build a server configuration based on this project and deploy it to the garnix cloud.","typ":{"innerType":{"fields":{"command":{"description":"The command to run to startthe server in production.","example":"server --port \"$PORT\"","name":"server command","typ":{"tag":"nonEmptyStr"}},"path":{"default":{"tag":"string","value":"/"},"description":"URL path your Haskell server will be hosted on.","name":"API path","typ":{"tag":"nonEmptyStr"}},"port":{"default":{"tag":"int","value":7000},"description":"Port to forward incoming HTTP requests to. This port has to be opened by the server command. This also sets the PORT environment variable for the server command.","example":"7000","typ":{"tag":"unsignedInt16"}}},"tag":"submodule"},"tag":"nullOr"}}},"tag":"submodule"},"tag":"attrsOf"}}},"tag":"submodule"}}');

INSERT INTO modules
  (repo_user, repo_name, git_commit, enabled, name, description, schema)
  VALUES
  ('garnix-io', 'nodejs-module', '8cbae0b9ab52f10e5ec4ceccdc1ae8c7ea062b13',
   true, 'NodeJS', 'A garnix module for projects using NodeJS.

Add dependencies, run tests, and optionally deploy a web server. Can be used either for frontend servers and backend servers.

[Documentation](https://garnix.io/docs/modules/nodejs) - [Source](https://github.com/garnix-io/nodejs-module).
', '{"description":"A garnix module for projects using NodeJS.\n\nAdd dependencies, run tests, and optionally deploy a web server. Can be used either for frontend servers and backend servers.\n\n[Documentation](https://garnix.io/docs/modules/nodejs) - [Source](https://github.com/garnix-io/nodejs-module).\n","typ":{"fields":{"nodejs":{"description":"An attrset of NodeJS projectsto generate.","typ":{"fieldType":{"fields":{"buildDependencies":{"default":{"tag":"list","value":[]},"description":"A list of additional dependencies required to build this package. They are made available in the devshell, and at build time.\n\n(It''s not necessary to include library dependencies manually, these will be included automatically.)\n","typ":{"elementType":{"tag":"package"},"tag":"listOf"}},"devTools":{"default":{"tag":"list","value":[]},"description":"A list of packages make available in the devshell for this project. This is useful for things like LSPs, formatters, etc.","name":"development tools","typ":{"elementType":{"tag":"package"},"tag":"listOf"}},"prettier":{"default":{"tag":"bool","value":false},"description":"Whether to create a CI check with prettier, and add it to the devshells.","typ":{"tag":"bool"}},"runtimeDependencies":{"default":{"tag":"list","value":[]},"description":"A list of dependencies required at runtime. They are made available in the devshell, at build time, and are available on the server at runtime.","typ":{"elementType":{"tag":"package"},"tag":"listOf"}},"src":{"description":"A path to the directory containing `package.json`, `package.lock`, and `src`.","example":"./.","name":"source directory","typ":{"tag":"path"}},"testCommand":{"default":{"tag":"string","value":"npm run test"},"description":"The command to run the test.","typ":{"tag":"str"}},"webServer":{"default":{"tag":"null"},"description":"Whether to build a server configuration based on this project and deploy it to the garnix cloud.","typ":{"innerType":{"fields":{"command":{"description":"The command to run to start the server in production.","example":"server --port \"$PORT\"","name":"server command","typ":{"tag":"nonEmptyStr"}},"path":{"default":{"tag":"string","value":"/"},"description":"Path your NodeJS server will be hosted on.","name":"API path","typ":{"tag":"nonEmptyStr"}},"port":{"default":{"tag":"int","value":3000},"description":"Port to forward incoming HTTP requests to. The server command has to listen on this port. This also sets the PORT environment variable for the server command.","typ":{"tag":"unsignedInt16"}}},"tag":"submodule"},"tag":"nullOr"}}},"tag":"submodule"},"tag":"attrsOf"}}},"tag":"submodule"}}');

INSERT INTO modules
  (repo_user, repo_name, git_commit, enabled, name, description, schema)
  VALUES
  ('garnix-io', 'postgresql-module', '25bed2780a50dffaab4e326aab368efa393a0fc1',
   true, 'PostgreSQL', 'A garnix module for projects using PostgreSQL.

Note: Enabling PostgreSQL will automatically mark the deployed server as [persistent](https://garnix.io/docs/hosting/persistence).

[Documentation](https://garnix.io/docs/modules/postgresql) - [Source](https://github.com/garnix-io/postgresql-module).
', '{"description":"A garnix module for projects using PostgreSQL.\n\nNote: Enabling PostgreSQL will automatically mark the deployed server as [persistent](https://garnix.io/docs/hosting/persistence).\n\n[Documentation](https://garnix.io/docs/modules/postgresql) - [Source](https://github.com/garnix-io/postgresql-module).\n","typ":{"fields":{"postgresql":{"description":"An attrset of PostgreSQL databases.","typ":{"fieldType":{"fields":{"port":{"default":{"tag":"int","value":5432},"description":"The porton which to run PostgreSQL.","typ":{"tag":"unsignedInt16"}}},"tag":"submodule"},"tag":"attrsOf"}}},"tag":"submodule"}}');

INSERT INTO modules
  (repo_user, repo_name, git_commit, enabled, name, description, schema)
  VALUES
  ('garnix-io', 'rust-module', '92af1490c377be715c070ee8582fcd859e2a6a70',
   true, 'Rust', 'A garnix module for projects using Rust.

Add build and runtime dependencies and optionally deploy a web server.

[Documentation](https://garnix.io/docs/modules/rust) - [Source](https://github.com/garnix-io/rust-module).
', '{"description":"A garnix module for projects using Rust.\n\nAdd build and runtime dependencies and optionally deploy a webserver.\n\n[Documentation](https://garnix.io/docs/modules/rust) - [Source](https://github.com/garnix-io/rust-module).\n","typ":{"fields":{"rust":{"description":"An attrset of Rust projects to generate.","typ":{"fieldType":{"fields":{"buildDependencies":{"default":{"tag":"list","value":[]},"description":"A list of additional dependencies required to build this package. They are made available in the devshell, and at build time.\n\n(It''s not necessary to include library dependencies manually, these willbe included automatically.)\n","typ":{"elementType":{"tag":"package"},"tag":"listOf"}},"devTools":{"default":{"tag":"list","value":[]},"description":"A list of packages make available in the devshell for this project (and `default` devshell). This is useful for things like LSPs, formatters, etc.","name":"development tools","typ":{"elementType":{"tag":"package"},"tag":"listOf"}},"runtimeDependencies":{"default":{"tag":"list","value":[]},"description":"A list of dependencies required at runtime. They aremade available in the devshell, at build time, and are available on the server at runtime.","typ":{"elementType":{"tag":"package"},"tag":"listOf"}},"src":{"description":"A path to the directory containing `Cargo.lock`, `Cargo.toml`, and `src`.","example":"./.","name":"source directory","typ":{"tag":"path"}},"webServer":{"default":{"tag":"null"},"description":"Whether to build aserver configuration based on this project and deploy it to the garnix cloud.","typ":{"innerType":{"fields":{"command":{"description":"The command to run to start the server in production.","example":"server --port \"$PORT\"","name":"server command","typ":{"tag":"nonEmptyStr"}},"path":{"default":{"tag":"string","value":"/"},"description":"Path your Rust server will be hosted on.","name":"API path","typ":{"tag":"nonEmptyStr"}},"port":{"default":{"tag":"int","value":8000},"description":"Port to forward incoming HTTP requests to. The server command has to listen on this port. This also sets the PORT environment variable for the server command.","example":"8000","typ":{"tag":"unsignedInt16"}}},"tag":"submodule"},"tag":"nullOr"}}},"tag":"submodule"},"tag":"attrsOf"}}},"tag":"submodule"}}');

INSERT INTO modules
  (repo_user, repo_name, git_commit, enabled, name, description, schema)
  VALUES
  ('garnix-io', 'user-module', '53cbc0bcfe8025248e7d77250449730cf93f7a98',
   true, 'User', 'A garnix module for adding Linux users and allowing remote access through `SSH`.

[Documentation](https://garnix.io/docs/modules/user) - [Source](https://github.com/garnix-io/user-module).
', '{"description":"A garnix module for adding Linux users and allowing remote access through `SSH`.\n\n[Documentation](https://garnix.io/docs/modules/user) - [Source](https://github.com/garnix-io/user-module).\n","typ":{"fields":{"user":{"description":"An attrset of users.","typ":{"fieldType":{"fields":{"authorizedSshKeys":{"description":"The public SSH keys that can be used to log in as this user. (Note that you must\n            use the IP address rather than domain for SSH.)","name":"SSH keys","typ":{"elementType":{"tag":"nonEmptyStr"},"tag":"listOf"}},"groups":{"default":{"tag":"list","value":[]},"description":"The groupsthe user belongs to.","example":"wheel","typ":{"elementType":{"tag":"str"},"tag":"listOf"}},"shell":{"default":{"tag":"string","value":"bash"},"description":"The users login shell.","typ":{"tag":"enum","variants":["bash","zsh","fish"]}},"user":{"description":"The Linux username.","example":"alice","name":"user name","typ":{"tag":"nonEmptyStr"}}},"tag":"submodule"},"tag":"attrsOf"}}},"tag":"submodule"}}');

INSERT INTO modules
  (repo_user, repo_name, git_commit, enabled, name, description, schema)
  VALUES
  ('garnix-io', 'rss-bridge-module', 'c07cdf51153c6a209191e88ffb1b0940469eb083',
   true, 'RSS-Bridge', 'A garnix module for projects using rss-bridge.

[Source](https://github.com/garnix-io/rss-bridge-module).
', '{"description":"A garnix module for projects using rss-bridge.\n\n[Source](https://github.com/garnix-io/rss-bridge-module).\n","typ":{"fields":{"rss-bridge":{"description":"An attrset of rss-bridge instances.","typ":{"fieldType":{"fields":{"path":{"default":{"tag":"string","value":"/"},"description":"Webserver path to host your rss-bridge server on.","typ":{"tag":"nonEmptyStr"}}},"tag":"submodule"},"tag":"attrsOf"}}},"tag":"submodule"}}');

SELECT * FROM modules;

