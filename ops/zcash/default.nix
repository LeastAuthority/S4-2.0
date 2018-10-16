/*
 * Zcash 2.0.1 aka Zcash Sapling
 */
{ lib, stdenv, pkgs, fetchFromGitHub, rustPlatform, gmp, libsodium, boost, callPackage }:
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
  rec { version = "2.0.1";
        name = "zcashd-" + version;
        src = fetchFromGitHub
        { owner = "zcash";
          repo = "zcash";
          rev = "v" + version;
          sha256 = "0dr7q1s3fwdrzh50l49xaaybnihdfx1m89dhmjnklwcfyyada62p";
        };
        patchPhase = old.patchPhase + ''
          # Cargo-culted based on upstream patchPhase.  Have not verified it
          # is really necessary.  Getting static boost may have been
          # sufficient to fix the issues that seemed related to this.
          sed -i"" 's,-lboost_program_options-mt,-lboost_program_options,' configure.ac

          # The libsnark gtest build fails with link errors I don't really
          # understand.  Skip the whole libsnark gtest build, instead.
          sed -i"" 's,$(CXX) -o $@   $(GTEST_OBJS) $(LIBSNARK_A) $(CXXFLAGS) $(LDFLAGS) $(GTEST_LDLIBS) $(LDLIBS),,' src/snark/Makefile
        '';

        postInstall =
        let extraPath = lib.makeBinPath
            [ # Provides flock, required by zcash-fetch-params.
              pkgs.utillinux
              # Also required by zcash-fetch-params.
              pkgs.wget
            ];
        in
        ''
        ${old.postInstall}
        # wrapProgram will only wrap things that are executable.
        chmod u+x $out/bin/zcash-fetch-params

        # Add utillinux (flock) and wget to fetch-params path since it tries
        # to use them.
        wrapProgram $out/bin/zcash-fetch-params --prefix PATH : ${extraPath}
        '';

        buildInputs =
        # Include a helper to fix zcash-fetch-params.
        [ pkgs.makeWrapper ] ++
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
