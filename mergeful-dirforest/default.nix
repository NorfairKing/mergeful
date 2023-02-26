{ mkDerivation, aeson, autodocodec, base, dirforest, lib, mergeful
, validity
}:
mkDerivation {
  pname = "mergeful-dirforest";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base dirforest mergeful validity
  ];
  homepage = "https://github.com/NorfairKing/mergeful#readme";
  license = lib.licenses.mit;
}
