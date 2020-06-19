{ mkDerivation, aeson, base, http-types, mtl, persistent
, persistent-sqlite, persistent-template, resource-pool, servant
, servant-server, stdenv, text, warp
}:
mkDerivation {
  pname = "budget";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base http-types mtl persistent persistent-sqlite
    persistent-template resource-pool servant servant-server text warp
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
