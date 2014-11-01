# nix build, attempted on Ubuntu 13.10, cant get past buildPhase:
let
  pkgs = import <nixpkgs> {};
  inherit (pkgs) stdenv gtk makeWrapper pkgconfig automake autoconf gtk_doc python libtool glibc;
#  inherit (pkgs) gnome mesa xlibs;
 # inherit (pkgs.gtkLibs) gtkmm glib;

# nixpkgs / pkgs / misc / themes / gtk2 / gtk-engine-murrine / default.nix
#  nixpkgs / pkgs / misc / themes / gtk2 / gtk-engines / default.nix


#inherit (gnome) GConf;

# very good search for info on how to build this.
#https://github.com/search?utf8=%E2%9C%93&q=--with-gtk%3D2+unity-gtk-module&type=Code&ref=searchresults

in stdenv.mkDerivation {
	  name = "unity-gtk-module";
	  src = (import <nixpkgs> {}).fetchurl {
	    url = https://launchpad.net/ubuntu/saucy/+source/unity-gtk-module/0.0.0+13.10.20130716.1-0ubuntu1/+files/unity-gtk-module_0.0.0+13.10.20130716.1.orig.tar.gz;
	    sha256 = "02f7b1e54f2404e395092206a2b8a0099a195b2bdb27ab4f9ef91b545e5d4755";
	  #  url = http://mirrors.kernel.org/ubuntu/pool/main/u/unity-gtk-module/unity-gtk2-module_0.0.0+13.10.20130716.1-0ubuntu1_amd64.deb;
	   # sha256 = "98ab2c6860f2de0591d7ebe6cbb7e658bb55b34acc10253067ff294c411a0fe9";
	  };

	  buildInputs = [ gtk python pkgconfig libtool automake autoconf gtk_doc];

    configureFlags = "--with-gtk=2";

    configureScript = "./autogen.sh";

	#preConfigure = ''
	#./autogen.sh $configureFlags
	#'';

    

#set environment variable. i think.
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


