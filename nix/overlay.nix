final: previous:
with final.lib;
with final.haskell.lib;
let
  mergefulPkg = name:
    doBenchmark (
      buildStrictly (
        final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
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

  mergefulRelease =
    final.symlinkJoin {
      name = "mergeful-release";
      paths = attrValues final.mergefulPackages;
    };

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          composeExtensions (old.overrides or (_: _: { })) (
            self: super: final.mergefulPackages
          );
      }
    );
}
