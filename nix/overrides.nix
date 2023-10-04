{ lib
, haskell
, symlinkJoin
, ...
}:
with lib;
with haskell.lib;
self: super:
let
  mergefulPkg = name: doBenchmark (buildStrictly (self.callPackage (../${name}) { }));
  mergefulPackages =
    {
      mergeful = mergefulPkg "mergeful";
      genvalidity-mergeful = mergefulPkg "genvalidity-mergeful";
      mergeful-persistent = mergefulPkg "mergeful-persistent";
    };
in
{
  inherit mergefulPackages;
  mergefulRelease = symlinkJoin {
    name = "mergeful-release";
    paths = attrValues self.mergefulPackages;
  };
} // mergefulPackages
