{ mkDerivation, aeson, base, base64-bytestring, bytestring
, containers, entropy, hspec, hspec-wai, hspec-wai-json, http-types
, magic-wormhole, mtl, network, network-uri, pgp-wordlist, saltine
, scientific, servant-server, spake2, stdenv, stm, text, time
, urlencoded, utf8-string, wai, wai-extra, warp
}:
mkDerivation {
  pname = "s4";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring containers entropy
    magic-wormhole mtl network network-uri pgp-wordlist saltine
    scientific servant-server spake2 stm text time urlencoded
    utf8-string wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base base64-bytestring bytestring hspec hspec-wai
    hspec-wai-json http-types network-uri saltine time urlencoded
    utf8-string wai-extra
  ];
  homepage = "https://github.com/LeastAuthority/s4#readme";
  license = stdenv.lib.licenses.bsd3;
}
