final:
  previous:
    with final.haskell.lib;
    {
      mergefulPackages = 
        { mergeful = failOnAllWarnings (final.haskellPackages.callCabal2nix "mergeful" (../mergeful) {});
          genvalidity-mergeful = failOnAllWarnings (final.haskellPackages.callCabal2nix "genvalidity-mergeful" (../genvalidity-mergeful) {});
        };
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
          self: super: final.mergefulPackages
        );
      });
    }
