let
  pk = import <nixpkgs> {};
  inherit ((import <nixpkgs> {}).gnome3) at_spi2_atk;
in
{ stdenv ? pk.stdenv, fetchurl ? pk.fetchurl,pkgconfig ? pk.pkgconfig, gettext ? pk.gettext, perl ? pk.perl
, expat ? pk.expat, glib ? pk.glib, cairo ? pk.cairo, pango ? pk.pango, gdk_pixbuf ? pk.gdk_pixbuf, atk ? pk.atk, gobjectIntrospection ? pk.gobjectIntrospection
, xlibs ? pk.xlibs, x11 ? pk.x11, wayland ? pk.wayland, libxkbcommon ? pk.libxkbcommon
, xineramaSupport ? pk.stdenv.isLinux
, cupsSupport ? pk.stdenv.isLinux, cups ? pk.cups
}:

assert xineramaSupport -> xlibs.libXinerama != null;
assert cupsSupport -> cups != null;

let
  ver_maj = "3.12";
  ver_min = "2";
  version = "${ver_maj}.${ver_min}";
in
stdenv.mkDerivation rec {
  name = "gtk+3-${version}";

  src = fetchurl {
    url = "mirror://gnome/sources/gtk+/${ver_maj}/gtk+-${version}.tar.xz";
    sha256 = "1l45nd7ln2pnrf99vdki3l7an5wrzkbak11hnnj1w6r3fkm4xmv1";
  };

  nativeBuildInputs = [ pkgconfig gettext gobjectIntrospection perl ];

  buildInputs = [ glib libxkbcommon ];
  propagatedBuildInputs = with xlibs; with stdenv.lib;
    [ expat cairo pango gdk_pixbuf atk at_spi2_atk ]
    ++ optionals stdenv.isLinux [ libXrandr libXrender libXcomposite libXi libXcursor wayland ]
    ++ optional stdenv.isDarwin x11
    ++ optional xineramaSupport libXinerama
    ++ optional cupsSupport cups;

  # demos fail to install, no idea where's the problem
  preConfigure = "sed '/^SRC_SUBDIRS /s/demos//' -i Makefile.in";

  enableParallelBuilding = true;

  postInstall = "rm -rf $out/share/gtk-doc";

  meta = {
    description = "A multi-platform toolkit for creating graphical user interfaces";

    longDescription = ''
      GTK+ is a highly usable, feature rich toolkit for creating
      graphical user interfaces which boasts cross platform
      compatibility and an easy to use API.  GTK+ it is written in C,
      but has bindings to many other popular programming languages
      such as C++, Python and C# among others.  GTK+ is licensed
      under the GNU LGPL 2.1 allowing development of both free and
      proprietary software with GTK+ without any license fees or
      royalties.
    '';

    homepage = http://www.gtk.org/;

    license = stdenv.lib.licenses.lgpl2Plus;

    maintainers = with stdenv.lib.maintainers; [ urkud raskin vcunat];
    platforms = stdenv.lib.platforms.all;
  };
}

