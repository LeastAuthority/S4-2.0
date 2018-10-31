{ mkDerivation, aeson, base, bytestring, containers, hspec
, hspec-wai, hspec-wai-json, http-types, monad-extras, network
, servant-server, stdenv, text, wai, wai-extra, warp
}:
mkDerivation {
  pname = "s4";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers monad-extras network servant-server text wai
    warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base bytestring hspec hspec-wai hspec-wai-json http-types
    text wai-extra
  ];
  homepage = "https://github.com/LeastAuthority/s4#readme";
  license = stdenv.lib.licenses.bsd3;
}
