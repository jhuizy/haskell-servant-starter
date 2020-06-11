{ mkDerivation, aeson, base, http-types, mtl, persistent
, persistent-sqlite, persistent-template, scotty, servant
, servant-server, stdenv, text, warp
}:
mkDerivation {
  pname = "budget";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base http-types mtl persistent persistent-sqlite
    persistent-template scotty servant servant-server text warp
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
