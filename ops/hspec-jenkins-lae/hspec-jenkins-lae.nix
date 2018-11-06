{ mkDerivation, base, fetchFromGitHub, blaze-markup, hspec, stdenv }:
mkDerivation {
  pname = "hspec-jenkins-lae";
  version = "0.1.2";
  src = fetchFromGitHub
  { owner = "LeastAuthority";
    repo = "hspec-jenkins";
    rev = "2899766bbd36216b81b1719423a5c2443458ae06";
    sha256 = "1w5kcg5n025q4whckyhjgz59i4z4k24isscaawpllfmllc29cjxl";
  };
  libraryHaskellDepends = [ base blaze-markup hspec ];
  homepage = "https://github.com/worksap-ate/hspec-jenkins";
  description = "Jenkins-friendly XML formatter for Hspec";
  license = stdenv.lib.licenses.mit;
}
