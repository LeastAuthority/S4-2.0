{ pkgs, fetchFromGitHub, rustPlatform, callPackage }:
let librustzcash = callPackage ./librustzcash.nix { };
    mapIf = cond: f: xs: map (x: if (cond x) then (f x) else x) xs;
    updatelibrustzcash = lib: librustzcash;
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
          (x: builtins.match "librustzcash-unstable-2017-03-17" x.name != null)
          updatelibrustzcash
          pkgs.altcoins.zcash.buildInputs;
    }
)
