{ mkDerivation, base, containers, directory, dismantle-ppc
, dismantle-tablegen, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "dismantle-coverage";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory dismantle-ppc dismantle-tablegen
    optparse-applicative text
  ];
  description = "Coverage tool for various ISAs";
  license = stdenv.lib.licenses.bsd3;
}
