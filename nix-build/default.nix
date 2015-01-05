# nix build tested on Ubuntu 13.10
#
#arguments for the nix expression, syntax is { argname ? defaultvalue, argname ? defaultvalue }
let
  pkgs = import <nixpkgs> {};

	   wxGTK = pkgs.stdenv.lib.overrideDerivation pkgs.wxGTK30 (oldAttrs: {
	     name = "wxWidgets-3.0.2-snapshot";
	     src = ../../wxWidgets-3.0.2;
	  });
  
  inherit (pkgs) gtk gnome xlibs mesa makeWrapper wxGTK30 haskell;
  inherit (haskell) stdenv ghc783Binary ghc742Binary ghcHEADPrefs;

  unityGtkModule = import ../unityGtkModule/saucybin.nix { inherit pkgs; };

  haskellPackages_ghc784 = pkgs.haskellPackages_ghc784.override {
   extension = self: super: {
     cabal = pkgs.haskellPackages_ghc784.cabalNoTest;
      
    # ghcPkgLib = import ./ghcPkgLib {
    #   inherit pkgs haskellPackages_ghc784 cabal cabalInstall;
     #};
 
     #Note: for buildwrapper to function, ghc, buildWrapper and cabalInstall must have the same Cabal library version.
     cabalInstall_1_18_0_8 = pkgs.callPackage ./cabalInstall/default.nix { 
        inherit (haskellPackages_ghc784) cabal Cabal filepath HTTP HUnit mtl network QuickCheck random stm testFramework testFrameworkHunit testFrameworkQuickcheck2 zlib;
     };

     cabalInstall = self.cabalInstall_1_18_0_8;

     buildwrapper = import ./buildwrapper {
        #must build Buildwrapper with the version of cabalInstall which matches the version of Cabal that ghc is dependent on.  If ghc is upgraded this will need to change to match.  Buildwrapper may require a different version of the Cabal library to be available at runtime than during build-time. The version of Cabal that cabalInstall was built with must be available at runtime. Use these commands to determine your current settings:  "cabal info Cabal", "cabal --version", "buildwrapper --version"

      
       inherit pkgs haskellPackages_ghc784 cabal cabalInstall;
     };

     wxdirect = import ./wxdirect {
       inherit pkgs haskellPackages_ghc784 cabal cabalInstall;
     };

     wxc = import ./wxc {
       inherit pkgs haskellPackages_ghc784 cabal cabalInstall wxGTK wxdirect;
     };

     wxcore = import ./wxcore {
       inherit pkgs haskellPackages_ghc784 cabal cabalInstall wxc wxdirect wxGTK;
     };

     wx = import ./wx {
       inherit pkgs haskellPackages_ghc784 cabal cabalInstall wxcore;
    };
  
     reactiveBananaWx = import ./reactiveBananaWx {
       inherit pkgs haskellPackages_ghc784 cabal cabalInstall wxcore wx;
    };

   };
};

  inherit (haskellPackages_ghc784) cabal cabalInstall
	      executablePath random split filepath reactiveBanana wxdirect wxc wxcore wx reactiveBananaWx buildwrapper Cabal;

#  inherit (pkgs.gtkLibs) gtkmm;
   

# we are not using gtk3. libcanberra_gtk3 fails to load.
  gtk_modules = [ pkgs.libcanberra unityGtkModule ];
  UBUNTU_MENUPROXY=1;
  

in cabal.mkDerivation (self: {
	  pname = "mud-frp";
	  version = "0.1.0.0";
	  src = ../.;
	  buildDepends = [ executablePath random split filepath reactiveBanana wxcore wx reactiveBananaWx makeWrapper buildwrapper Cabal];
	  extraLibraries = [ xlibs.libX11 wxGTK gtk mesa ];

	  buildTools = [ cabalInstall buildwrapper]; 
	  enableSplitObjs = false;

	  isLibrary = false;
	  isExecutable = true;
        
     # GTK_MODULES="overlay-scrollbar:unity-gtk-module";  #required setting for proper UI in UBUNTU (at least)
     gtk_modules = map (x: x + x.gtkModule) gtk_modules;
	#set the Language environment variable to English to avoid the warning,
        #during cabal configure, that "ghc can't determine gcc version" 
	#this will make gcc use English.
	#see https://ghc.haskell.org/trac/ghc/ticket/8825 about this warning
	  LANGUAGE="en_US:en";
          dontStrip=1;

#      doExport2 = ''
#export PATH="/usr/lib/lightdm/lightdm:${cabalInstall_1_18_0_3}/bin:/usr/bin/X11:$PATH"
#'';

#--prefix PATH : /usr/lib/lightdm/lightdm:${cabalInstall_1_18_0_3}/bin:/usr/bin/X11

      postInstall = ''
        wrapProgram $out/bin/mudFrp --set UBUNTU_MENUPROXY 1 --suffix-each GTK_PATH ':' "$gtk_modules"; 
      '';
	})
	



#http://stackoverflow.com/questions/21007052/gtk-warning-unable-to-locate-theme-engine-in-module-path-murrine-error-whi
