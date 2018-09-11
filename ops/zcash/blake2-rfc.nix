{ pkgs, fetchFromGitHub, rustPlatform }:
rustPlatform.buildRustPackage
  rec {
    name = "blake2-rfc";
    version = "0.2.18";
    src = fetchFromGitHub {
      owner = "gtank";
      repo = "blake2-rfc";
      rev = "9c044fb4344f7e0d51a7c327f16417a207a2cb8e";
      sha256 = "05wa6hf1lgd50d90iiq9qr3dwd3gcz0643dm0v8nfbmmr295sgbq";
      };
    cargoSha256 = "124zij7xalqzzcm65pb9ipjfvlv1darbc8x8a4n27lh2qc2f3yc6";

    cargoPatches = [ (pkgs.writeText "cargo-lock.patch" ''
--- /dev/null   2018-08-28 14:38:09.697791309 -0400
+++ $sourceRoot/Cargo.lock  2018-09-10 15:25:10.738705683 -0400
@@ -0,0 +1,76 @@
+[[package]]
+name = "arrayvec"
+version = "0.4.7"
+source = "registry+https://github.com/rust-lang/crates.io-index"
+dependencies = [
+ "nodrop 0.1.12 (registry+https://github.com/rust-lang/crates.io-index)",
+]
+
+[[package]]
+name = "blake2-rfc"
+version = "0.2.18"
+dependencies = [
+ "arrayvec 0.4.7 (registry+https://github.com/rust-lang/crates.io-index)",
+ "clippy 0.0.41 (registry+https://github.com/rust-lang/crates.io-index)",
+ "constant_time_eq 0.1.3 (registry+https://github.com/rust-lang/crates.io-index)",
+ "data-encoding 2.1.1 (registry+https://github.com/rust-lang/crates.io-index)",
+]
+
+[[package]]
+name = "clippy"
+version = "0.0.41"
+source = "registry+https://github.com/rust-lang/crates.io-index"
+dependencies = [
+ "regex-syntax 0.2.6 (registry+https://github.com/rust-lang/crates.io-index)",
+ "semver 0.2.3 (registry+https://github.com/rust-lang/crates.io-index)",
+ "unicode-normalization 0.1.7 (registry+https://github.com/rust-lang/crates.io-index)",
+]
+
+[[package]]
+name = "constant_time_eq"
+version = "0.1.3"
+source = "registry+https://github.com/rust-lang/crates.io-index"
+
+[[package]]
+name = "data-encoding"
+version = "2.1.1"
+source = "registry+https://github.com/rust-lang/crates.io-index"
+
+[[package]]
+name = "nodrop"
+version = "0.1.12"
+source = "registry+https://github.com/rust-lang/crates.io-index"
+
+[[package]]
+name = "nom"
+version = "1.2.4"
+source = "registry+https://github.com/rust-lang/crates.io-index"
+
+[[package]]
+name = "regex-syntax"
+version = "0.2.6"
+source = "registry+https://github.com/rust-lang/crates.io-index"
+
+[[package]]
+name = "semver"
+version = "0.2.3"
+source = "registry+https://github.com/rust-lang/crates.io-index"
+dependencies = [
+ "nom 1.2.4 (registry+https://github.com/rust-lang/crates.io-index)",
+]
+
+[[package]]
+name = "unicode-normalization"
+version = "0.1.7"
+source = "registry+https://github.com/rust-lang/crates.io-index"
+
+[metadata]
+"checksum arrayvec 0.4.7 (registry+https://github.com/rust-lang/crates.io-index)" = "a1e964f9e24d588183fcb43503abda40d288c8657dfc27311516ce2f05675aef"
+"checksum clippy 0.0.41 (registry+https://github.com/rust-lang/crates.io-index)" = "00c794772d29bc7d447f7760345e4c45549a2b4fac4cb7dcc88b5d86fcc2d00d"
+"checksum constant_time_eq 0.1.3 (registry+https://github.com/rust-lang/crates.io-index)" = "8ff012e225ce166d4422e0e78419d901719760f62ae2b7969ca6b564d1b54a9e"
+"checksum data-encoding 2.1.1 (registry+https://github.com/rust-lang/crates.io-index)" = "67df0571a74bf0d97fb8b2ed22abdd9a48475c96bd327db968b7d9cace99655e"
+"checksum nodrop 0.1.12 (registry+https://github.com/rust-lang/crates.io-index)" = "9a2228dca57108069a5262f2ed8bd2e82496d2e074a06d1ccc7ce1687b6ae0a2"
+"checksum nom 1.2.4 (registry+https://github.com/rust-lang/crates.io-index)" = "a5b8c256fd9471521bcb84c3cdba98921497f1a331cbc15b8030fc63b82050ce"
+"checksum regex-syntax 0.2.6 (registry+https://github.com/rust-lang/crates.io-index)" = "a21935ce5a4dfa48e3ded1aefbbe353fb9ab258b0d3fa0bd168bef00797b3dc7"
+"checksum semver 0.2.3 (registry+https://github.com/rust-lang/crates.io-index)" = "2d5b7638a1f03815d94e88cb3b3c08e87f0db4d683ef499d1836aaf70a45623f"
+"checksum unicode-normalization 0.1.7 (registry+https://github.com/rust-lang/crates.io-index)" = "6a0180bc61fc5a987082bfa111f4cc95c4caff7f9799f3e46df09163a937aa25"
'') ];
}
