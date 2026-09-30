{ mkDerivation, ansi-terminal, async, attoparsec, base, bytestring
, cassava, containers, directory, doctest-parallel, extra, filelock
, filepath, fsnotify, hermes-json, HUnit, lib, optics, random
, relude, safe, safe-exceptions, stm, streamly-core, strict
, template-haskell, terminal-size, text, time, transformers
, typed-process, unix, word8
}:
mkDerivation {
  pname = "nix-output-monitor";
  version = "2.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal async attoparsec base bytestring cassava containers
    directory extra filelock filepath fsnotify hermes-json optics
    relude safe safe-exceptions stm streamly-core strict
    template-haskell terminal-size text time transformers word8
  ];
  executableHaskellDepends = [
    ansi-terminal async attoparsec base bytestring cassava containers
    directory extra filelock filepath fsnotify hermes-json optics
    relude safe safe-exceptions stm streamly-core strict
    template-haskell terminal-size text time transformers typed-process
    unix word8
  ];
  testHaskellDepends = [
    ansi-terminal async attoparsec base bytestring cassava containers
    directory doctest-parallel extra filelock filepath fsnotify
    hermes-json HUnit optics random relude safe safe-exceptions stm
    streamly-core strict template-haskell terminal-size text time
    transformers typed-process word8
  ];
  homepage = "https://code.maralorn.de/maralorn/nix-output-monitor";
  description = "Processes output of Nix commands to show helpful and pretty information";
  license = lib.meta.getLicenseFromSpdxId "EUPL-1.2";
  mainProgram = "nom";
}
