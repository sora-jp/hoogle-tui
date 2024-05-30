{
  inputs = {
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          (haskellPackages.ghcWithPackages.override {withHoogle = false;} (p: with p; [hoogle brick microlens microlens-th microlens-mtl vty text vector command text-zipper]))
        ];
      };
      packages.default = pkgs.haskellPackages.developPackage {
        root = ./.;
        withHoogle = false;
      };
    }
  );
}
