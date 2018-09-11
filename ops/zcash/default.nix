{ pkgs, fetchFromGitHub, rustPlatform, callPackage }:
let blake2rfc = callPackage ./blake2-rfc.nix {};
    mapIf = cond: f: xs: map (x: if (cond x) then (f x) else x) xs;
    updatelibrustzcash =
      (lib: lib.overrideAttrs (old:
      rec {
        version = "2018-09-10";
        name = "librustzcash-unstable-${version}";
        src = fetchFromGitHub {
          owner = "zcash";
          repo = "librustzcash";
          rev = "e1c6232dd7a5368aa9342649d73db27522cc8d6e";
          sha256 = "1dc249kj039kwyi3hy2dqikq5h3m0an95ffhkdngrq1yivn240h6";
        };
        buildInputs = [ blake2rfc ];
        patches = [ (pkgs.writeText "blake2rfc.patch" ''
diff --git a/librustzcash/Cargo.toml b/librustzcash/Cargo.toml
index 0494c07..d29ce28 100644
--- $sourceRoot/librustzcash/Cargo.toml
+++ $sourceRoot/librustzcash/Cargo.toml
@@ -21,7 +21,4 @@ lazy_static = "1"
 byteorder = "1"
 rand = "0.4"
 sapling-crypto = { path = "../sapling-crypto" }
-
-[dependencies.blake2-rfc]
-git = "https://github.com/gtank/blake2-rfc"
-rev = "7a5b5fc99ae483a0043db7547fb79a6fa44b88a9"
+blake2-rfc = { path = "${blake2rfc}" }
'') ];
      }));
in
pkgs.altcoins.zcash.overrideAttrs (old:
rec { version = "2.0.0";
      name = "zcashd-" + version;
      src = fetchFromGitHub
      { owner = "zcash";
        repo = "zcash";
        rev = "v2.0.0";
        sha256 = "0n18amzk96dncrvar5w8wxz75is2gjmrm643k37rxd8z2k1m9rbj";
      };
      buildInputs =
      with pkgs;
        mapIf
          (x: builtins.match "librustzcash-unstable-.*" x.name != null)
          updatelibrustzcash
          pkgs.altcoins.zcash.buildInputs;
    }
)
