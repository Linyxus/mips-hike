{ mkDerivation, array, base, bytestring, containers, lens
, megaparsec, mtl, optparse-applicative, parser-combinators, stdenv
, text
}:
mkDerivation {
  pname = "model";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    array base bytestring containers lens megaparsec mtl
    optparse-applicative parser-combinators text
  ];
  license = stdenv.lib.licenses.bsd3;
}
