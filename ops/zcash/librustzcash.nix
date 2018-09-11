{ stdenv, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  version = "2018-09-10";
  name = "librustzcash-unstable-${version}";

  src = fetchFromGitHub {
    owner = "zcash";
    repo = "librustzcash";
    rev = "e1c6232dd7a5368aa9342649d73db27522cc8d6e";
    sha256 = "1dc249kj039kwyi3hy2dqikq5h3m0an95ffhkdngrq1yivn240h6";
  };

  cargoSha256 = "0vcfj7h6279s1a2fv2m7mzl60rv20jnk4rqrn68jg9spxcndslpc";

  prePatch = ''
cat >>../.cargo/config <<EOF
[source."https://github.com/gtank/blake2-rfc"]
git = "https://github.com/gtank/blake2-rfc"
rev = "7a5b5fc99ae483a0043db7547fb79a6fa44b88a9"
replace-with = "vendored-sources"
EOF
'';

  installPhase = ''
    mkdir -p $out/lib
    cp target/release/librustzcash.a $out/lib/
    mkdir -p $out/include
    cp include/librustzcash.h $out/include/
  '';

  meta = with stdenv.lib; {
    description = "Rust-language assets for Zcash";
    homepage = https://github.com/zcash/librustzcash;
    maintainers = with maintainers; [ rht ];
    license = with licenses; [ mit asl20 ];
    platforms = platforms.unix;
  };
}
