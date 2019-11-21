{ mkDerivation, aeson, base, bytestring, case-insensitive
, classy-prelude, classy-prelude-conduit, classy-prelude-yesod
, conduit, containers, data-default, directory, fast-logger
, file-embed, foreign-store, hjsmin, hpack, hspec, http-client-tls
, http-conduit, lens, lens-aeson, microlens, mime-mail
, monad-control, monad-logger, persistent, persistent-sqlite
, persistent-template, safe, scientific, shakespeare, stdenv
, template-haskell, text, time, unordered-containers, vector, wai
, wai-extra, wai-logger, warp, wreq, yaml, yesod, yesod-auth
, yesod-core, yesod-form, yesod-static, yesod-test, yesod-bin
}:
mkDerivation {
  pname = "rememberportal";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = false;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit containers
    data-default directory fast-logger file-embed foreign-store hjsmin
    http-client-tls http-conduit lens lens-aeson mime-mail
    monad-control monad-logger persistent persistent-sqlite
    persistent-template safe scientific shakespeare template-haskell
    text time unordered-containers vector wai wai-extra wai-logger warp
    wreq yaml yesod yesod-auth yesod-core yesod-form yesod-static
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit containers
    data-default directory fast-logger file-embed foreign-store hjsmin
    http-client-tls http-conduit lens lens-aeson mime-mail
    monad-control monad-logger persistent persistent-sqlite
    persistent-template safe scientific shakespeare template-haskell
    text time unordered-containers vector wai wai-extra wai-logger warp
    wreq yaml yesod yesod-auth yesod-core yesod-form yesod-static yesod-bin
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit containers
    data-default directory fast-logger file-embed foreign-store hjsmin
    hspec http-client-tls http-conduit lens lens-aeson microlens
    mime-mail monad-control monad-logger persistent persistent-sqlite
    persistent-template safe scientific shakespeare template-haskell
    text time unordered-containers vector wai wai-extra wai-logger warp
    wreq yaml yesod yesod-auth yesod-core yesod-form yesod-static
    yesod-test
  ];
  preConfigure = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
  postInstall = "ln -s $out/share/*-*-*/rememberportal-*/static $out/static";
}
