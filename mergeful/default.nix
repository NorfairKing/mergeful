{ mkDerivation, aeson, autodocodec, base, containers, deepseq, lib
, mtl, text, time, validity, validity-containers, validity-time
}:
mkDerivation {
  pname = "mergeful";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base containers deepseq mtl text time validity
    validity-containers validity-time
  ];
  homepage = "https://github.com/NorfairKing/mergeful#readme";
  license = lib.licenses.mit;
}
