{ mkDerivation, base, Cabal, containers, filemanip, filepath, lib
, optparse-applicative, process
}:
mkDerivation {
  pname = "ghcicabal";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base Cabal containers filemanip filepath optparse-applicative
    process
  ];
  homepage = "https://github.com/choener/ghcicabal";
  description = "todo";
  license = lib.licenses.gpl3Plus;
}
