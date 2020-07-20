{ mkDerivation, aeson, base, bytestring, http-types, network
, postgresql-typed, stdenv, text, uuid, wai, wai-extra, warp
}:
mkDerivation {
  pname = "hello";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring network postgresql-typed text uuid
  ];
  executableHaskellDepends = [
    aeson base http-types text uuid wai wai-extra warp
  ];
  license = "MIT";
  hydraPlatforms = stdenv.lib.platforms.none;
}
