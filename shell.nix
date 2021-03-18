{ sources ? (import ./nix/sources.nix { })
, pkgs ? import sources.nixpkgs { }
}:

with pkgs;

let
  easy-purescript-nix = import sources.easy-purescript-nix { inherit pkgs; };

in
  stdenv.mkDerivation {
    name = "purescript-halogen-markdown";
    src = ./.;
    buildInputs = with easy-purescript-nix; [
      purs
      spago
      nodejs-15_x
      yarn
    ];
  }
