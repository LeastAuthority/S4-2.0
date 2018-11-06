{ mkDerivation, aeson, async, base, base64-bytestring, bytestring
, containers, either, entropy, hspec, hspec-jenkins, hspec-wai
, hspec-wai-json, http-types, magic-wormhole, monad-extras, network
, network-uri, pgp-wordlist, safe-exceptions, servant-server
, spake2, stdenv, text, url, utf8-string, wai, wai-extra, warp
}:
mkDerivation {
  pname = "s4";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base base64-bytestring bytestring containers entropy
    magic-wormhole monad-extras network network-uri pgp-wordlist
    safe-exceptions servant-server spake2 text url utf8-string wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base base64-bytestring bytestring either hspec hspec-jenkins
    hspec-wai hspec-wai-json http-types network-uri safe-exceptions
    text url utf8-string wai-extra
  ];
  homepage = "https://github.com/LeastAuthority/s4#readme";
  license = stdenv.lib.licenses.bsd3;
}
