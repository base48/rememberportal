{ mkDerivation, aeson, base, bytestring, case-insensitive
, classy-prelude, classy-prelude-conduit, classy-prelude-yesod
, conduit, containers, data-default, directory, fast-logger
, file-embed, foreign-store, hjsmin, hpack, hspec, http-client-tls
, http-conduit, microlens, mime-mail, monad-control, monad-logger
, persistent, persistent-sqlite, persistent-template, safe
, shakespeare, stdenv, template-haskell, text, time
, unordered-containers, vector, wai, wai-extra, wai-logger, warp
, yaml, yesod, yesod-auth, yesod-core, yesod-form, yesod-static
, yesod-test
}:
mkDerivation {
  pname = "rememberportal";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit containers
    data-default directory fast-logger file-embed foreign-store hjsmin
    http-client-tls http-conduit mime-mail monad-control monad-logger
    persistent persistent-sqlite persistent-template safe shakespeare
    template-haskell text time unordered-containers vector wai
    wai-extra wai-logger warp yaml yesod yesod-auth yesod-core
    yesod-form yesod-static
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit containers
    data-default directory fast-logger file-embed foreign-store hjsmin
    http-client-tls http-conduit mime-mail monad-control monad-logger
    persistent persistent-sqlite persistent-template safe shakespeare
    template-haskell text time unordered-containers vector wai
    wai-extra wai-logger warp yaml yesod yesod-auth yesod-core
    yesod-form yesod-static
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit containers
    data-default directory fast-logger file-embed foreign-store hjsmin
    hspec http-client-tls http-conduit microlens mime-mail
    monad-control monad-logger persistent persistent-sqlite
    persistent-template safe shakespeare template-haskell text time
    unordered-containers vector wai wai-extra wai-logger warp yaml
    yesod yesod-auth yesod-core yesod-form yesod-static yesod-test
  ];
  preConfigure = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
