# nix build tested on Ubuntu 13.10
#
# cant use yet: cabalInstall_1_20_0_3
#arguments for the nix expression, syntax is { argname ? defaultvalue, argname ? defaultvalue }
let
  pkgs = import <nixpkgs> {};
in
{ fontconfig ? pkgs.fontconfig, makeWrapper ? pkgs.makeWrapper, haskellPackages ? pkgs.haskellPackages, 
  pkgconfig ? pkgs.pkgconfig, gtkmm ? pkgs.gtkmm, glibmm ? pkgs.glibmm, gtk_modules ? [ pkgs.libcanberra ] }: (
        let
	  inherit (haskellPackages) cabal cabalInstall_1_18_0_3
	    executablePath random filepath wx wxcore reactiveBanana reactiveBananaWx;

	in cabal.mkDerivation (self: {
	  pname = "mud-frp";
	  version = "0.1.0.0";
	  src = ./.;
	  buildDepends = [ pkgconfig glibmm gtkmm fontconfig cabalInstall_1_18_0_3 executablePath random filepath wx wxcore reactiveBanana reactiveBananaWx makeWrapper];
	  buildTools = [ cabalInstall_1_18_0_3 ];
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

      doExport2 = ''
export PATH="/usr/lib/lightdm/lightdm:${fontconfig}/bin:${cabalInstall_1_18_0_3}/bin:/usr/bin/X11:$PATH"
'';
#--set GTK_MODULES overlay-scrollbar:unity-gtk-module
      postInstall = ''
        wrapProgram $out/bin/mudFrp --suffix-each GTK_PATH ':' "$gtk_modules"  --prefix PATH : /usr/lib/lightdm/lightdm:${fontconfig}/bin:${cabalInstall_1_18_0_3}/bin:/usr/bin/X11''; 

	})
)


#http://stackoverflow.com/questions/21007052/gtk-warning-unable-to-locate-theme-engine-in-module-path-murrine-error-whi
