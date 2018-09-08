{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ zlib ];
  buildPhase = ''
    export LANG=en_US.UTF-8
    '';
}
