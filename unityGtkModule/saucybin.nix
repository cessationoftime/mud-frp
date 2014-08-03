# nix build, attempted on Ubuntu 13.10
let
  pkgs = import <nixpkgs> {};
in
{ dpkg ? pkgs.dpkg, fetchurl ? pkgs.fetchurl, stdenv ? pkgs.stdenv, makeWrapper ? pkgs.makeWrapper }: (
        

#http://packages.ubuntu.com/saucy/unity-gtk3-module
#http://packages.ubuntu.com/saucy/amd64/unity-gtk3-module/download

  stdenv.mkDerivation {
	  name = "unity-gtk-module";
	  src = fetchurl {
	    url = http://mirrors.kernel.org/ubuntu/pool/main/u/unity-gtk-module/unity-gtk3-module_0.0.0+13.10.20130716.1-0ubuntu1_amd64.deb;
	    sha256 = "2ff868384a86de0647727503139a28adfaa92e7574bb274d5b84b86337a5419b";
	  };

	  buildInputs = [ dpkg ];
          unpackPhase = "true";
          dontStrip = true;
          dontPatchELF = true;

	  passthru = {
	    gtkModule = "/usr/lib/x86_64-linux-gnu/gtk-3.0/";
	  };

	installPhase = ''
	  mkdir -p $out
	  dpkg-deb -x $src $out
	''; # */

	  meta = {
	    description = "The unity gtk module";

	    homepage = https://launchpad.net/unity-gtk-module;

	    longDescription = ''
	      GTK+ module for exporting old-style menus as GMenuModels.

	      Many applications implement menus as GtkMenuShells and GtkMenuItems and aren't looking to migrate to the newer GMenuModel API.

	      This GTK+ module watches for these types of menus and exports the appropriate GMenuModel implementation.
	    '';
	  };
  
}

)
