{
  description = "yet another llm wrapper";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      ghc = "ghc96";
    in
    {
      overlays.default =
        final: prev:
        let
          inherit (final.haskell.lib) unmarkBroken doJailbreak;
        in
        {
          haskell = prev.haskell // {
            packages = prev.haskell.packages // {
              ${ghc} = prev.haskell.packages.${ghc}.override {
                overrides = self: super: {
                  json-rpc = unmarkBroken super.json-rpc;
                  terminal-coffkell = self.callCabal2nix "terminal-coffkell" ./. { };
                };
              };
            };
          };
          terminal-coffkell = final.haskell.lib.justStaticExecutables final.haskell.packages.${ghc}.terminal-coffkell;
        };

      legacyPackages = forAllSystems (
        system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        }
      );

      packages = forAllSystems (system: {
        default = self.packages.${system}.terminal-coffkell;
        terminal-coffkell = self.legacyPackages.${system}.terminal-coffkell;
      });

      devShells = forAllSystems (system:{
        default = self.legacyPackages.${system}.haskell.packages.${ghc}.shellFor {
          withHoogle = true;
          packages = p: [
            p.terminal-coffkell
          ];
          buildInputs =
            with self.legacyPackages.${system};
            [
              ghcid
              texliveSmall
              # (texlive.combine { inherit (texlive) scheme-small listings;})
            ]
            ++ (with haskell.packages.${ghc}; [
              cabal-install
              haskell-language-server
              cabal-gild
              hlint
              weeder
            ]);
        };
        order =
          let
            pkgs = self.legacyPackages.${system};
          in
          pkgs.shellFor {
            packages = with pkgs; [
                (ghc.withPackages (ps: [ ps.terminal-coffkell ]))
                texliveSmall
                texlivePackages.listing
            ];
            COFFEETIME = "yes please";
          };
      });

      checks = forAllSystems (system: {
        inherit (self.packages.${system}) terminal-coffkell;
      });
    };
}
