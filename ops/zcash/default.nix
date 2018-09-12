/*
 * Zcash 2.0.0 aka Zcash Sapling
 */
{ pkgs, fetchFromGitHub, rustPlatform, callPackage }:
/*
 * Get a sufficiently up-to-date version of librustzcash
 */
let librustzcash = callPackage ./librustzcash.nix { };
    /* Replace a library with our updated librustzcash */
    updatelibrustzcash = lib: librustzcash;
    /*
     * Map either a given function or identity over a list.  Use the given
     * function if a given predicate is true, identity otherwise.
     *
     * cond: The predicate
     * f: The mapping function
     * xs: The list
     */
    mapIf = cond: f: xs: map (x: if (cond x) then (f x) else x) xs;
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
        /*
         * In the old build inputs, replace the old librustzcash with our new
         * one.
         */
        mapIf
          (x: builtins.match "librustzcash-unstable-2017-03-17" x.name != null)
          updatelibrustzcash
          altcoins.zcash.buildInputs;
    }
)
