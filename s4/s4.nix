{ mkDerivation, base, hspec, hspec-jenkins-lae, stdenv }:
mkDerivation {
  pname = "s4";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec hspec-jenkins-lae ];
  homepage = "https://github.com/LeastAuthority/s4#readme";
  license = stdenv.lib.licenses.bsd3;
}
