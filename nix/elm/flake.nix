{
  description = "Elm";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.default =
      let
        pkgs = import nixpkgs {
          system = "x86_64-linux";
        };

        releases = [
          {
            version = "0.19.3";
            name = "elm-0.19.3-linux-x64";
            sha = "sha256-0RcCsxV37YtyC52BxSyKGaUrcrH8+t7w/K2S8heYxYo=";
          }
          {
            version = "0.19.2";
            name = "elm-0.19.2-linux-x64";
            sha = "sha256-ZjINJ3AWVPoRvQ6NhL35gpaU1XcMjc7i3t5hYPrVhzc=";
          }
          {
            version = "0.19.1";
            name = "binary-for-linux-64-bit";
            sha = "sha256-5Er1K7J/clqXNHjlidmQpkKOEV/huxTwODMTTWwPFVw=";
          }
          {
            version = "0.19.0";
            name = "binary-for-linux-64-bit";
            sha = "sha256-01mtvuiYI8ZBzaMmk4cI1yJ9x5qh8WLg2P4nXxgvUoo=";
          }
        ];

        getAndInstallElmVersion =
          release:
          pkgs.stdenv.mkDerivation {
            pname = "elm-${release.version}";
            version = release.version;

            src = pkgs.fetchurl {
              url = "https://github.com/elm/compiler/releases/download/${release.version}/${release.name}.gz";
              sha256 = release.sha;
            };

            unpackPhase = ''
              cp $src elm.gz
              gzip -d elm.gz
            '';

            installPhase = ''
              install -Dm755 elm $out/bin/elm-${release.version}
            '';
          };

        elmVersions = map getAndInstallElmVersion releases;
      in
      pkgs.stdenv.mkDerivation {
        pname = "elm";
        version = "0.19.3";
        nativeBuildInputs = elmVersions;

        src = pkgs.writeText "elm" ''
          #!/usr/bin/env bash

          # A script that automatically runs the correct Elm version based on elm.json.
          # Assumes the following binaries in $PATH: 
          # elm-0.19.0
          # elm-0.19.1
          # elm-0.19.2
          # etc...
          # elm-latest (the latest version, used for packages and as a fallback)

          # Find the closest elm.json.
          dir="$(pwd)"
          while true; do
            if test -f "$dir/elm.json"; then
              break
            fi
            if test "$dir" = '/'; then
              # No elm.json exists. Fall back to the latest version.
              elm-latest "$@"
              exit $?
            fi
            dir="$(dirname "$dir")"
          done

          if grep -qE '"type"\s*:\s*"package"' "$dir/elm.json"; then
            # Run the latest Elm for packages.
            elm-latest "$@"
          else
            # Read the Elm version for applications.
            version="$(grep -P '\"elm-version\"\s*:\s*\"\d+\.\d+\.\d+\"' "$dir/elm.json" | cut -d '"' -f 4)"
            
            if test -z "$version"; then
              # No version found in elm.json. Fall back to the latest version.
              elm-latest "$@"
            else
              "elm-$version" "$@"
            fi
          fi'';

        dontUnpack = true;

        installPhase = ''
          mkdir -p $out/bin

          # Install the wrapper.
          install -Dm755 $src $out/bin/elm

          # Copy all versioned Elm binaries.
          for elm in ${pkgs.lib.concatStringsSep " " elmVersions}; do
            cp "$elm/bin/"* "$out/bin/"
          done

          # The newest version is the fallback.
          ln -s elm-0.19.3 $out/bin/elm-latest
        '';
      };
  };
}
