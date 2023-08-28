{
  description = "OCaml environment";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell
          {
            packages = [
              pkgs.clang_16
              pkgs.ocaml
              pkgs.opam
              pkgs.dune_3
              pkgs.ocamlPackages.ocaml-lsp
              pkgs.ocamlPackages.ocamlformat
              pkgs.ocamlPackages.ocamlbuild
            ];

            shellHook = ''
              eval $(opam env)
            '';
          };
      });
}
