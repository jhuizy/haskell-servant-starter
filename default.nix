{ mkDerivation, base, scotty, stdenv }:
mkDerivation {
  pname = "budget";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base scotty ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
