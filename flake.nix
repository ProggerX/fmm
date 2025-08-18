{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs =
    { flake-parts, nixpkgs, ... }@inputs:
    let
      hs-project =
        {
          pkgs,
          isShell ? false,
        }:
        pkgs.haskellPackages.developPackage {
          root = ./.;
          returnShellEnv = isShell;
          modifier =
            drv:
            pkgs.haskell.lib.addBuildTools drv (
              with pkgs;
              [
                cabal-install
                haskell-language-server
                wrapGAppsHook4
              ]
            );
        };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.platforms.unix;
      perSystem =
        { pkgs, ... }:
        {
          packages.default = hs-project { inherit pkgs; };
          devShells.default =
            let
              p = hs-project {
                inherit pkgs;
                isShell = true;
              };
            in
            p.overrideAttrs {
              shellHook = with pkgs; ''
                XDG_DATA_DIRS=$XDG_DATA_DIRS:${gtk4}/share/gsettings-schemas/${gtk4.name}
              '';
            };
        };
    };
}
