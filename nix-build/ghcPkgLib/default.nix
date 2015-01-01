{ pkgs, haskellPackages, cabal, cabalInstall }:

let

inherit (haskellPackages) filepath Cabal ghc ghcPaths;

in cabal.mkDerivation (self: {
    pname = "ghcPkgLib";
    version = "0.2.1";
    src = ../../../ghc-pkg-lib;
    buildDepends = [ filepath Cabal ghc ghcPaths ];
    buildTools = [ cabalInstall ];


    isLibrary = true;
    isExecutable = false;
        
  #set the Language environment variable to English to avoid the warning,
  #during cabal configure, that "ghc can't determine gcc version" 
  #this will make gcc use English.
  #see https://ghc.haskell.org/trac/ghc/ticket/8825 about this warning
    LANGUAGE="en_US:en";
          dontStrip=1;
  })
