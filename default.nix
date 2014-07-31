# nix build tested on Ubuntu 13.10
#
# cant use yet: cabalInstall_1_20_0_3
#arguments for the nix expression, syntax is { argname ? defaultvalue, argname ? defaultvalue }
let
  pkgs = import <nixpkgs> {};
in
{ haskellPackages ? pkgs.haskellPackages }: (
        let
	  inherit (haskellPackages) cabal cabalInstall_1_18_0_3
	    executablePath random filepath wx wxcore reactiveBanana reactiveBananaWx;

	in cabal.mkDerivation (self: {
	  pname = "mud-frp";
	  version = "0.1.0.0";
	  src = ./.;
	  buildDepends = [ executablePath random filepath wx wxcore reactiveBanana reactiveBananaWx ];
	  buildTools = [ cabalInstall_1_18_0_3 ];
	  enableSplitObjs = false;

	  isLibrary = false;
	  isExecutable = true;
        
        GTK_MODULES="overlay-scrollbar:unity-gtk-module";  #required setting for proper UI in UBUNTU (at least)

	#set the Language environment variable to English to avoid the warning,
        #during cabal configure, that "ghc can't determine gcc version" 
	#this will make gcc use English.
	#see https://ghc.haskell.org/trac/ghc/ticket/8825 about this warning
	  LANGUAGE="en_US:en";

      doExport2 = ''
export PATH="/home/cvanvranken/.cabal/bin:/usr/bin/X11:$PATH"
      '';

   #   postInstall = ''
     #   makeWrapper $out/bin/mudFrp --prefix LD_LIBRARY_PATH : ${self.stdenv.gcc.gcc}/lib
    #  ''; 

	})
)

