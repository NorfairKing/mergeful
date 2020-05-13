final: previous:
with final.haskell.lib;
let
  mergefulPkg = name:
    doBenchmark (
      failOnAllWarnings (
        final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) {}
      )
    );

in
{
  mergefulPackages =
    {
      mergeful = mergefulPkg "mergeful";
      genvalidity-mergeful = mergefulPkg "genvalidity-mergeful";
      mergeful-persistent = mergefulPkg "mergeful-persistent";
    };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (old.overrides or (_: _: {})) (
              self: super: final.mergefulPackages
            );
        }
    );
}
