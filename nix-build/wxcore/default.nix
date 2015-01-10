{ pkgs, haskellPackagesModified, cabal, cabalInstall, wxc, wxdirect, wxGTK }:

let 

inherit (pkgs) mesa xlibs;

inherit (haskellPackagesModified) filepath parsec stm time;

in cabal.mkDerivation (self: {
    pname = "wxcore";
    version = "0.92.0.0";
    src = ../../../wxHaskell/wxcore;
    buildDepends = [ filepath parsec stm time wxc wxdirect ];
    extraLibraries = [ xlibs.libX11 mesa wxGTK ];
    buildTools = [ cabalInstall ];

    isLibrary = true;
    isExecutable = false;


    patchPhase = ''
     sed -i -e 's|array >= .*,|array,|' wxcore.cabal
    '';

  #set the Language environment variable to English to avoid the warning,
  #during cabal configure, that "ghc can't determine gcc version" 
  #this will make gcc use English.
  #see https://ghc.haskell.org/trac/ghc/ticket/8825 about this warning
    LANGUAGE="en_US:en";
          dontStrip=1;
  })
