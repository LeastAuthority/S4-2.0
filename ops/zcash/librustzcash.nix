/*
 * librustzcash, a build dependency of Zcash
 */
{ stdenv, fetchFromGitHub, rustPlatform }:
rustPlatform.buildRustPackage rec {
  version = "2018-10-11";
  name = "librustzcash-unstable-${version}";

  src = fetchFromGitHub {
    owner = "zcash";
    repo = "librustzcash";

    # This is the revision specified by <zcash/depends/librustzcash.mk>.  For
    # minimum surprises, we want to stick with the version that the upstream
    # build scripts select.
    rev = "f5e5cb24e1bd756a02fc4a3fd2b824238ccd15ad";

    /*
     * Give the correct source hash to satisfy buildRustPackage that we're
     * building against the source we expected.
     */
    sha256 = "0dsdp00w43xdhbgq25px6h4k2fhm9zyy2s9wy1slr9y27rlcb0ab";
  };

  /*
   * Give the correct Cargo hash to satisfy buildRustPackage that we're
   * building against the dependencies we expected.
   */
  cargoSha256 = "18aax340ni7qr00vf19fdz9wvcwk849ka2m2d9wvwvf51kc0gcdq";

  /*
   * Replace the blake2-rfc git revision dependency with a vendored copy of
   * the very same source.  This is necessary to satisfy buildRustPackage
   * which otherwise attempts to regenerate Cargo.lock which is impossible
   * during a nix build.
   *
   * The cargo config file is first written by buildRustPackage itself to
   * accomplish much the same goal for all of the rest of the dependencies.
   * We only need to append this extra section to what's there already.
   */
#   prePatch = ''
# cat >>../.cargo/config <<EOF
# [source."https://github.com/gtank/blake2-rfc"]
# git = "https://github.com/gtank/blake2-rfc"
# rev = "7a5b5fc99ae483a0043db7547fb79a6fa44b88a9"
# replace-with = "vendored-sources"
# EOF
# '';

  checkPhase = ''
    runHook preCheck
    echo "Running cargo test --release"
    cargo test --release
    runHook postCheck
  '';
  /*
   * Install the static library that results from the build.  buildRustPackage
   * doesn't know how to deal with library artifacts so we're left to do this
   * ourselves.
   */
  installPhase = ''
    mkdir -p $out/lib
    cp target/release/librustzcash.a $out/lib/
    mkdir -p $out/include
    cp librustzcash/include/librustzcash.h $out/include/
  '';

  /*
   * Just copied from upstream Nix librustzcash package.
   */
  meta = with stdenv.lib; {
    description = "Rust-language assets for Zcash";
    homepage = https://github.com/zcash/librustzcash;
    maintainers = with maintainers; [ rht ];
    license = with licenses; [ mit asl20 ];
    platforms = platforms.unix;
  };
}
