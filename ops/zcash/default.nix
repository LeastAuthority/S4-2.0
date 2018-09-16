/*
 * Zcash 2.0.0 aka Zcash Sapling
 */
{ stdenv, pkgs, fetchFromGitHub, rustPlatform, gmp, libsodium, boost, callPackage }:
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
    zcash = pkgs.altcoins.zcash;
    overrides =
    /* Zcash wants to statically link to everything.
     */
    { libsodium = libsodium.overrideAttrs (old: { configureFlags = "--enable-static"; });
       gmp = gmp.override { withStatic = true; };
       boost = boost.override
       { enableStatic = true;
         enableSingleThreaded = true;
         enableMultiThreaded = false;
       };
    };
in
/*
 * Use as much of the upstream Nix Zcash package as we can, overriding just
 * what's necessary to get us to 2.0.0.
 */
  (zcash.override overrides).overrideAttrs (old:
  rec { version = "2.0.0";
        name = "zcashd-" + version;
        src = fetchFromGitHub
        { owner = "zcash";
          repo = "zcash";
          rev = "v2.0.0";
          sha256 = "0n18amzk96dncrvar5w8wxz75is2gjmrm643k37rxd8z2k1m9rbj";
        };
        patchPhase = old.patchPhase + ''
          # Cargo-culted based on upstream patchPhase.  Have not verified it
          # is really necessary.  Getting static boost may have been
          # sufficient to fix the issues that seemed related to this.
          sed -i"" 's,-lboost_program_options-mt,-lboost_program_options,' configure.ac
        '';

        buildInputs =
          /*
           * In the old build inputs, replace the old librustzcash with our
           * new one.  The usual (simpler) idiom for this replacement,
           * `zcash.override { ... }` does not work because the Zcash function
           * doesn't take librustzcash as an argument.  It loads it directly
           * using `callPackage`.
           */
          mapIf
            (x: builtins.match "librustzcash-unstable-2017-03-17" x.name != null)
            updatelibrustzcash
            old.buildInputs;
      }
 )
