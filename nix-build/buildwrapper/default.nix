{ pkgs, haskellPackages, cabal, cabalInstall }:

let

inherit (haskellPackages) filepath mtl Cabal dynamicCabal process regexTdfa ghc ghcPaths syb text vector
                   haskellSrcExts cpphs aeson unorderedContainers utf8String attoparsec transformers deepseq
                   conduit conduitExtra async cmdargs HUnit HTF;

in cabal.mkDerivation (self: {
    pname = "buildwrapper";
    version = "0.9.0";
    src = ../../../BuildWrapper;
    buildDepends = [ filepath mtl Cabal dynamicCabal process regexTdfa ghc ghcPaths syb text vector
                   haskellSrcExts cpphs aeson unorderedContainers utf8String attoparsec transformers deepseq
                   conduit conduitExtra async cmdargs HUnit HTF];
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
