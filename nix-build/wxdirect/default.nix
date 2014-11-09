{pkgs, haskellPackages, cabal, cabalInstall }:

let inherit (haskellPackages) parsec strict time filepath;

in cabal.mkDerivation (self: {
    pname = "wxdirect";
    version = "0.91.0.0";
    src = ../../../wxHaskell/wxdirect;
    buildDepends = [ filepath parsec strict time ];
    buildTools = [ cabalInstall ];
    enableSplitObjs = false;

    isLibrary = true;
    isExecutable = true;
    jailbreak = true;

   #set the Language environment variable to English to avoid the warning,
   #during cabal configure, that "ghc can't determine gcc version" 
   #this will make gcc use English.
   #see https://ghc.haskell.org/trac/ghc/ticket/8825 about this warning
    LANGUAGE="en_US:en";
          dontStrip=1;
  })