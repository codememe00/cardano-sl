# Running `nix-shell` on this file puts you in an environment where all the
# dependencies needed to work on `wallet-new` using `Cabal` are available.
#
# Running `nix-build` builds the `wallet-new` code.
#
# Since this file is intended for development purposes, by default we enable
# profiling for dependencies.

{ enableProfiling ? true
, enableDebugging ? true
}@args:

let
  drv = (import ../. args).cardano-sl-wallet-new;
in
  if ((import <nixpkgs> {}).stdenv.lib.inNixShell)
    then drv.env
    else drv
