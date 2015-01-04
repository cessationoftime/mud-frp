{ pkgs, haskellPackages_ghc784, cabal, cabalInstall, wxcore, wx }:

let

inherit (haskellPackages_ghc784) reactiveBanana;

in cabal.mkDerivation (self: {
pname = "reactive-banana-wx";
version = "0.8.0.1";
src = ../../../reactive-banana/reactive-banana-wx;
isLibrary = true;
isExecutable = true;
buildDepends = [ reactiveBanana wx wxcore ];
configureFlags = "-f-buildExamples";
jailbreak = true;
meta = {
homepage = "http://haskell.org/haskellwiki/Reactive-banana";
description = "Examples for the reactive-banana library, using wxHaskell";
license = self.stdenv.lib.licenses.bsd3;
platforms = self.ghc.meta.platforms;
};
})