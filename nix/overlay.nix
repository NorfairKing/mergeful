final: previous:
with final.haskell.lib;

{
  mergefulPackages =
    {
      mergeful =
        failOnAllWarnings (
          final.haskellPackages.callCabal2nix "mergeful" ( final.gitignoreSource ../mergeful ) {}
        );
      genvalidity-mergeful =
        failOnAllWarnings (
          final.haskellPackages.callCabal2nix "genvalidity-mergeful" ( final.gitignoreSource ../genvalidity-mergeful ) {}
        );
      mergeful-persistent =
        failOnAllWarnings (
          final.haskellPackages.callCabal2nix "mergeful-persistent" ( final.gitignoreSource ../mergeful-persistent ) {}
        );
    };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions ( old.overrides or (_: _: {}) ) (
              self: super: final.mergefulPackages
            );
        }
    );
}
