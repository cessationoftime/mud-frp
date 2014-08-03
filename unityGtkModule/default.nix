# nix build, attempted on Ubuntu 13.10, cant get past buildPhase:
let
  pkgs = import <nixpkgs> {};
  gtk3 = pkgs.callPackage ./gtk3 {};
in
{ stdenv ? pkgs.stdenv, makeWrapper ? pkgs.makeWrapper, pkgconfig ? pkgs.pkgconfig , automake ? pkgs.automake, autoconf ? pkgs.autoconf, gtk_doc ? pkgs.gtk_doc, python ? pkgs.python, libtool ? pkgs.libtool}: (
        
  stdenv.mkDerivation {
	  name = "unity-gtk-module";
	  src = (import <nixpkgs> {}).fetchurl {
	    url = https://launchpad.net/ubuntu/saucy/+source/unity-gtk-module/0.0.0+13.10.20130716.1-0ubuntu1/+files/unity-gtk-module_0.0.0+13.10.20130716.1.orig.tar.gz;
	    sha256 = "02f7b1e54f2404e395092206a2b8a0099a195b2bdb27ab4f9ef91b545e5d4755";
	  };

	  buildInputs = [ libtool pkgconfig python gtk_doc automake autoconf gtk3];

	preConfigure = ''
	./autogen.sh 
	'';

	  passthru = {
	    gtkModule = "/lib/gtk-2.0/";
	  };

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
