let
  pkgs = import ./nix/pkgs.nix;
  nix-pre-commit-hooks =
    import (
      builtins.fetchTarball "https://github.com/hercules-ci/nix-pre-commit-hooks/archive/a3bd860016653ab53ed49e2c4523e3e7297e58bb.tar.gz"
    );
  check =
    nix-pre-commit-hooks.run {
      src = pkgs.gitignoreSource ./.;
    };
in
  pkgs.mergefulPackages // {
    pre-commit-check = check;
  }
