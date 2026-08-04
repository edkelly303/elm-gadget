{
  description = "elm-ir";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    elm-0-19-2-pkg.url = "path:./nix/elm";
  };

  outputs =
    inputs:
    let
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs {
        system = system;
        config.allowUnfree = true;
      };
      elm-0-19-2 = inputs.elm-0-19-2-pkg.defaultPackage.${system};
    in
    {
      # SHELL
      devShells.${system}.default = pkgs.mkShell {
        name = "devShell";
        packages = with pkgs; [
          git
          xdg-utils
          nodejs
          # use our own flake for Elm until nixpkgs has 0.19.2
          # elmPackages.elm
          elm-0-19-2
          elmPackages.elm-json
          elmPackages.elm-format
          elmPackages.elm-test
          elmPackages.elm-doc-preview
          elmPackages.elm-review
        ];
        shellHook = ''
          DEVDIR="$PWD"
          echo -e "\n\033[1m*** Entering development shell for elm-gadget ***\033[0m\n"

          echo -n "Updating repos... "
          if cd $DEVDIR && git pull --quiet; then
            echo -e "Success!\n"
          else
            echo -e "Failed!\n"
          fi

          git config --local core.hooksPath "$DEVDIR/.githooks/"
          chmod +x "$DEVDIR/.githooks/pre-commit"

          echo -e "\033[1;36mrun\033[0m: start the development environment"

          run () {
            cd "$DEVDIR"
            code .
            (sleep 2; xdg-open 'http://localhost:8007') &
            npx run-pty run-pty.json
          }
        '';
      };
    };
}
