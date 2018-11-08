{ mkDerivation, base, fetchFromGitHub, blaze-markup, hspec, stdenv }:
mkDerivation {
  pname = "hspec-jenkins-lae";
  version = "0.1.2";
  src = fetchFromGitHub
  { owner = "LeastAuthority";
    repo = "hspec-jenkins";
    rev = "7d2274461a395f9fa889890e14c6d758314b3af3";
    sha256 = "0kp95dq50149zm4h9x1yi36w56hbh2nalgcgnynbj70s08x27w3m";
  };
  libraryHaskellDepends = [ base blaze-markup hspec ];
  homepage = "https://github.com/worksap-ate/hspec-jenkins";
  description = "Jenkins-friendly XML formatter for Hspec";
  license = stdenv.lib.licenses.mit;
}
