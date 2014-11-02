{ pkgs, haskellPackages, cabal, cabalInstall, wxcore }:

let

inherit (haskellPackages) stm;

in cabal.mkDerivation (self: {
    pname = "wx";
    version = "0.90.2.0";
    src = ../../../wxHaskell/wx;
    buildDepends = [ stm wxcore ];
    buildTools = [ cabalInstall ];


    isLibrary = true;
    isExecutable = false;

    preConfigure = "find . -type f -exec touch {} +";
        
  #set the Language environment variable to English to avoid the warning,
  #during cabal configure, that "ghc can't determine gcc version" 
  #this will make gcc use English.
  #see https://ghc.haskell.org/trac/ghc/ticket/8825 about this warning
    LANGUAGE="en_US:en";
          dontStrip=1;
  })