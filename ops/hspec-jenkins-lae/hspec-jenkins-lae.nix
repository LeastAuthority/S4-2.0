{ mkDerivation, base, fetchFromGitHub, blaze-markup, hspec, stdenv }:
mkDerivation {
  pname = "hspec-jenkins-lae";
  version = "0.1.2";
  src = fetchFromGitHub
  { owner = "LeastAuthority";
    repo = "hspec-jenkins";
    rev = "34556c238911afe28146110d049eed76ce600675";
    sha256 = "0ik0d3f0mk3zybsy7gmw8cr4misd3jl77qqf0mrsmz5mqpqsspwz";
  };
  libraryHaskellDepends = [ base blaze-markup hspec ];
  homepage = "https://github.com/worksap-ate/hspec-jenkins";
  description = "Jenkins-friendly XML formatter for Hspec";
  license = stdenv.lib.licenses.mit;
}
