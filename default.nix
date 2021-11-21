let
  pkgs = import ./nix/pkgs.nix { };
in
pkgs.mergefulPackages
