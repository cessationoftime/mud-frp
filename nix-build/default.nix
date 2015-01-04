# nix build tested on Ubuntu 13.10
#
# cant use yet: cabalInstall_1_20_0_3
#arguments for the nix expression, syntax is { argname ? defaultvalue, argname ? defaultvalue }
let
  pkgs = import <nixpkgs> {};

	   wxGTK = pkgs.stdenv.lib.overrideDerivation pkgs.wxGTK30 (oldAttrs: {
	     name = "wxWidgets-3.0.2-snapshot";
	     src = ../../wxWidgets-3.0.2;
	  });
  
  inherit (pkgs) gtk gnome xlibs mesa makeWrapper wxGTK30;

  unityGtkModule = import ../unityGtkModule/saucybin.nix { inherit pkgs; };

# ghc784Prefs = pkgs.haskell.ghc784Prefs.override {
#    cabalInstall_1_20_0_6 = super.cabalInstall_1_20_0_6.override { Cabal = self.Cabal_1_20_0_2; };
    #codex = super.codex.override { hackageDb = super.hackageDb.override { Cabal = self.Cabal_1_20_0_2; }; };
    #jailbreakCabal = super.jailbreakCabal.override { Cabal = self.Cabal_1_20_0_2; };
    #MonadRandom = self.MonadRandom_0_2_0_1; # newer versions require transformers >= 0.4.x
    #mtl = self.mtl_2_1_3_1;
    #transformersCompat = super.transformersCompat.override { cabal = self.cabal.override {
    #extension = self: super: { configureFlags = "-fthree " + super.configureFlags or ""; };
 #}; };
#};

#packages_ghc784 =
 # pkgs.haskell.packages { 
#  ghcPath = pkgs.development.compilers.ghc.7.8.4.nix;
  #ghcBinary = if stdenv.isDarwin then ghc783Binary else ghc742Binary;
  #prefFun = ghc784Prefs;
#};

  haskellPackages_ghc784 = pkgs.haskellPackages_ghc784.override {
   extension = self: super: {
     #cabalInstall = super.cabalInstall_1_20_0_4;
     #Cabal = super.Cabal_1_20_0_2;
     #aeson = super.aeson_0_7_0_4;
     #scientific = super.scientific_0_2_0_2;
      cabal = pkgs.haskellPackages_ghc784.cabalNoTest;
      
    # ghcPkgLib = import ./ghcPkgLib {
    #   inherit pkgs haskellPackages_ghc784 cabal cabalInstall;
     #};
 
     buildwrapper = import ./buildwrapper {
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
	  buildDepends = [ cabalInstall executablePath random split filepath reactiveBanana wxcore wx reactiveBananaWx makeWrapper buildwrapper Cabal];
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
