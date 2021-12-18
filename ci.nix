let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit-hooks = import ./nix/pre-commit.nix { inherit sources; };
in
pkgs.mergefulPackages // {
  "pre-commit-hooks" = pre-commit-hooks.run;
}
