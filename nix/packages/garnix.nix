{ mkDerivation
, aeson
, aeson-pretty
, aeson-qq
, amazonka
, amazonka-s3
, attoparsec
, auto-update
, autodocodec
, autodocodec-schema
, autodocodec-yaml
, base
, base64-bytestring
, binary
, bytestring
, casing
, containers
, cookie
, cradle
, crypton
, data-default-class
, directory
, exceptions
, extra
, fast-logger
, file-embed
, filepath
, format-numbers
, generic-lens
, generic-random
, generics-eot
, getopt-generics
, github
, github-app
, github-webhooks
, hashable
, hashids
, hashtables
, hspec
, hspec-core
, hspec-discover
, hspec-golden
, hspec-golden-aeson
, http-client
, http-client-tls
, http-conduit
, http-media
, http-types
, HUnit
, interpolate
, iso-deriving
, jose
, jwt
, lens
, lens-aeson
, lens-regex-pcre
, lib
, lifted-async
, lifted-base
, mockery
, monad-control
, mtl
, neat-interpolation
, network-uri
, nix-derivation
, oauth2-simple
, openapi3
, pcre-light
, port-utils
, postgresql-typed
, pretty-show
, prettyprinter
, process
, prometheus
, QuickCheck
, quickcheck-instances
, random
, resource-pool
, resourcet
, retry
, row-types
, row-types-aeson
, safe-exceptions
, servant
, servant-auth
, servant-auth-server
, servant-github-webhook
, servant-rawm
, servant-rawm-server
, servant-server
, shake
, silently
, stm
, streaming
, streaming-bytestring
, string-conversions
, strip-ansi-escape
, systemd
, tagged
, template-haskell
, temporary
, text
, time
, tls
, transformers-base
, turtle
, typed-process
, unix
, unordered-containers
, uuid
, vector
, wai
, wai-app-static
, wai-extra
, warp
, wreq
, yaml
}:
mkDerivation {
  pname = "garnix";
  version = "0.1.0.0";
  src = ../../backend;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    aeson-pretty
    aeson-qq
    amazonka
    amazonka-s3
    attoparsec
    auto-update
    autodocodec
    autodocodec-schema
    base
    base64-bytestring
    bytestring
    casing
    containers
    cookie
    cradle
    crypton
    data-default-class
    directory
    exceptions
    extra
    fast-logger
    file-embed
    filepath
    format-numbers
    generic-lens
    generics-eot
    getopt-generics
    github
    github-app
    github-webhooks
    hashable
    hashids
    hashtables
    http-client
    http-client-tls
    http-conduit
    http-media
    http-types
    interpolate
    iso-deriving
    jose
    jwt
    lens
    lens-aeson
    lens-regex-pcre
    lifted-async
    lifted-base
    monad-control
    mtl
    network-uri
    nix-derivation
    oauth2-simple
    openapi3
    postgresql-typed
    pretty-show
    prettyprinter
    process
    prometheus
    random
    resource-pool
    resourcet
    retry
    row-types
    row-types-aeson
    safe-exceptions
    servant
    servant-auth
    servant-auth-server
    servant-github-webhook
    servant-rawm
    servant-rawm-server
    servant-server
    shake
    stm
    streaming
    streaming-bytestring
    string-conversions
    strip-ansi-escape
    systemd
    tagged
    template-haskell
    temporary
    text
    time
    tls
    transformers-base
    typed-process
    unix
    unordered-containers
    uuid
    vector
    wai
    wai-app-static
    wai-extra
    warp
    wreq
    yaml
  ];
  executableHaskellDepends = [
    aeson
    aeson-pretty
    aeson-qq
    amazonka
    amazonka-s3
    attoparsec
    auto-update
    autodocodec
    autodocodec-schema
    autodocodec-yaml
    base
    base64-bytestring
    bytestring
    casing
    containers
    cookie
    cradle
    crypton
    data-default-class
    directory
    exceptions
    extra
    fast-logger
    file-embed
    filepath
    format-numbers
    generic-lens
    generics-eot
    getopt-generics
    github
    github-app
    github-webhooks
    hashable
    hashids
    hashtables
    http-client
    http-client-tls
    http-conduit
    http-media
    http-types
    interpolate
    iso-deriving
    jose
    jwt
    lens
    lens-aeson
    lens-regex-pcre
    lifted-async
    lifted-base
    monad-control
    mtl
    network-uri
    nix-derivation
    oauth2-simple
    openapi3
    postgresql-typed
    pretty-show
    prettyprinter
    process
    prometheus
    random
    resource-pool
    resourcet
    retry
    row-types
    row-types-aeson
    safe-exceptions
    servant
    servant-auth
    servant-auth-server
    servant-github-webhook
    servant-rawm
    servant-rawm-server
    servant-server
    shake
    stm
    streaming
    streaming-bytestring
    string-conversions
    strip-ansi-escape
    systemd
    tagged
    template-haskell
    temporary
    text
    time
    tls
    transformers-base
    typed-process
    unix
    unordered-containers
    uuid
    vector
    wai
    wai-app-static
    wai-extra
    warp
    wreq
    yaml
  ];
  testHaskellDepends = [
    aeson
    aeson-pretty
    aeson-qq
    amazonka
    amazonka-s3
    attoparsec
    auto-update
    autodocodec
    autodocodec-schema
    base
    base64-bytestring
    binary
    bytestring
    casing
    containers
    cookie
    cradle
    crypton
    data-default-class
    directory
    exceptions
    extra
    fast-logger
    file-embed
    filepath
    format-numbers
    generic-lens
    generic-random
    generics-eot
    getopt-generics
    github
    github-app
    github-webhooks
    hashable
    hashids
    hashtables
    hspec
    hspec-core
    hspec-discover
    hspec-golden
    hspec-golden-aeson
    http-client
    http-client-tls
    http-conduit
    http-media
    http-types
    HUnit
    interpolate
    iso-deriving
    jose
    jwt
    lens
    lens-aeson
    lens-regex-pcre
    lifted-async
    lifted-base
    mockery
    monad-control
    mtl
    neat-interpolation
    network-uri
    nix-derivation
    oauth2-simple
    openapi3
    pcre-light
    port-utils
    postgresql-typed
    pretty-show
    prettyprinter
    process
    prometheus
    QuickCheck
    quickcheck-instances
    random
    resource-pool
    resourcet
    retry
    row-types
    row-types-aeson
    safe-exceptions
    servant
    servant-auth
    servant-auth-server
    servant-github-webhook
    servant-rawm
    servant-rawm-server
    servant-server
    shake
    silently
    stm
    streaming
    streaming-bytestring
    string-conversions
    strip-ansi-escape
    systemd
    tagged
    template-haskell
    temporary
    text
    time
    tls
    transformers-base
    turtle
    typed-process
    unix
    unordered-containers
    uuid
    vector
    wai
    wai-app-static
    wai-extra
    warp
    wreq
    yaml
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/jkarni/garnix#readme";
  license = lib.licenses.bsd3;
}
