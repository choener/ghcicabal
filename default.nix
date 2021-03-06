{ mkDerivation, base, Cabal, containers, filemanip, filepath
, optparse-applicative, process, stdenv
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
  license = stdenv.lib.licenses.gpl3Plus;
}
