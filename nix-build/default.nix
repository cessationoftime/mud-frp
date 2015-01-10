# Install mudFrp with: 
# nix-env -f default.nix -iA mudFrp

# Build  mudFrp with:
# nix-build default.nix -A mudFrp

# Install this version of cabal-install with: 
# nix-env -f default.nix -iA cabalInstall

# Install this version of buildWrapper with: 
# nix-env -f default.nix -iA buildwrapper

# Install this version of wx/wxcore: 
# nix-env -f default.nix -iA wx
# nix-env -f default.nix -iA wxcore



{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };
  
  callPackage = pkgs.lib.callPackageWith (pkgs // pkgs.xlibs // self);
  
   wxGTK = pkgs.stdenv.lib.overrideDerivation pkgs.wxGTK30 (oldAttrs: {
       name = "wxWidgets-3.0.2-snapshot";
       src = ../../wxWidgets-3.0.2;
    });
  
  
  inherit (pkgs.haskell) stdenv ghc783Binary ghc742Binary ghcHEADPrefs;

  self = rec { 
    inherit (haskellPackagesModified) cabal cabalInstall wxdirect wxc wxcore wx;

    haskellPackagesModified = pkgs.haskellPackages_ghc784.override {
     extension = self: super: {
           cabal = pkgs.haskellPackages_ghc784.cabalNoTest;
            
          # ghcPkgLib = import ./ghcPkgLib {
          #   inherit pkgs haskellPackages_ghc784 cabal cabalInstall;
           #};
       
           #Note: for buildwrapper to function, ghc, buildWrapper and cabalInstall must have the same Cabal library version.
           cabalInstall_1_18_0_8 = pkgs.callPackage ./cabalInstall/default.nix { 
              inherit (haskellPackagesModified) cabal Cabal filepath HTTP HUnit mtl network QuickCheck random stm testFramework testFrameworkHunit testFrameworkQuickcheck2 zlib;
           };

           cabalInstall = self.cabalInstall_1_18_0_8;

           buildwrapper = import ./buildwrapper {
              #must build Buildwrapper with the version of cabalInstall which matches the version of Cabal that ghc is dependent on.  If ghc is upgraded this will need to change to match.  Buildwrapper may require a different version of the Cabal library to be available at runtime than during build-time. The version of Cabal that cabalInstall was built with must be available at runtime. Use these commands to determine your current settings:  "cabal info Cabal", "cabal --version", "buildwrapper --version"

            
             inherit pkgs haskellPackagesModified cabal cabalInstall;
           };

           wxdirect = import ./wxdirect {
             inherit pkgs haskellPackagesModified cabal cabalInstall;
           };

           wxc = callPackage ./wxc { cabal=cabal; wxGTK=wxGTK; wxdirect=wxdirect; }; 

           wxcore = import ./wxcore {
             inherit pkgs haskellPackagesModified cabal cabalInstall wxc wxdirect wxGTK;
           };

           wx = import ./wx {
             inherit pkgs haskellPackagesModified cabal cabalInstall wxcore;
          };
        
           reactiveBananaWx = import ./reactiveBananaWx {
             inherit pkgs haskellPackagesModified cabal cabalInstall wxcore wx;
          };

       };
    };


    mudFrp = callPackage ./mudFrp { };
    buildwrapper = haskellPackagesModified.buildwrapper;
  };
in
self