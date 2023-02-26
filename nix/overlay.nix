final: prev:
with final.lib;
with final.haskell.lib;
{

  mergefulRelease =
    final.symlinkJoin {
      name = "mergeful-release";
      paths = attrValues final.haskellPackages.mergefulPackages;
    };

  haskellPackages =
    prev.haskellPackages.override (
      old:
      {
        overrides =
          composeExtensions (old.overrides or (_: _: { })) (
            self: super:
              let
                mergefulPkg = name:
                  doBenchmark (
                    buildStrictly (
                      self.callPackage (../${name}/default.nix) { }
                    )
                  );
                mergefulPackages =
                  {
                    mergeful = mergefulPkg "mergeful";
                    genvalidity-mergeful = mergefulPkg "genvalidity-mergeful";
                    mergeful-persistent = mergefulPkg "mergeful-persistent";
                    mergeful-dirforest = mergefulPkg "mergeful-dirforest";
                    genvalidity-mergeful-dirforest = mergefulPkg "genvalidity-mergeful-dirforest";
                  };
              in
              {
                inherit mergefulPackages;
              } // mergefulPackages
          );
      }
    );
}
