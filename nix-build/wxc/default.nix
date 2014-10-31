{pkgs, haskellPackages, cabal, cabalInstall, wxGTK, wxdirect }:

let

inherit (pkgs) gtk gnome mesa xlibs;

#inherit (gnome) GConf;

in cabal.mkDerivation (self: {
    pname = "wxc";
    version = "0.90.2.0";
    src = ../../../wxHaskell/wxc;
    buildDepends = [ wxdirect ];
    extraLibraries = [ xlibs.libX11 mesa wxGTK ];
    noHaddock = true;
    postInstall = ''
      cp -v dist/build/libwxc.so.${self.version} $out/lib/libwxc.so
    '';

    isLibrary = true;
    isExecutable = false;

   

  #set the Language environment variable to English to avoid the warning,
  #during cabal configure, that "ghc can't determine gcc version" 
  #this will make gcc use English.
  #see https://ghc.haskell.org/trac/ghc/ticket/8825 about this warning
    LANGUAGE="en_US:en";
          dontStrip=1;
  })