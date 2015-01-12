# nix build tested on Ubuntu 13.10
#
#arguments for the nix expression, syntax is { argname ? defaultvalue, argname ? defaultvalue }
{ pkgs, haskellPackagesModified, wxGTK }:
let

	
  inherit (pkgs) gtk gnome xlibs mesa makeWrapper wxGTK30 haskell;
  inherit (haskellPackagesModified) cabal cabalInstall
	      executablePath random split filepath reactiveBanana wxdirect wxc wxcore wx reactiveBananaWx buildwrapper Cabal concurrentExtra;

#  inherit (pkgs.gtkLibs) gtkmm;
  unityGtkModule = import ../../unityGtkModule/saucybin.nix { inherit pkgs; };

# we are not using gtk3. libcanberra_gtk3 fails to load.
  gtk_modules = [ pkgs.libcanberra unityGtkModule ];
  UBUNTU_MENUPROXY=1;
  

in cabal.mkDerivation (self: {
	  pname = "mud-frp";
	  version = "0.1.0.0";
	  src = ../../.;
	  buildDepends = [ executablePath random split filepath reactiveBanana wxcore wx reactiveBananaWx makeWrapper buildwrapper Cabal concurrentExtra];
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
