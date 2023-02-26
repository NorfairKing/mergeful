{ mkDerivation, base, genvalidity, genvalidity-dirforest
, genvalidity-mergeful, genvalidity-sydtest
, genvalidity-sydtest-aeson, lib, mergeful, mergeful-dirforest
, QuickCheck, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "genvalidity-mergeful-dirforest";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-dirforest genvalidity-mergeful
    mergeful-dirforest
  ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-sydtest-aeson mergeful
    mergeful-dirforest QuickCheck sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/mergeful#readme";
  license = lib.licenses.mit;
}
