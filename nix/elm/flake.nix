{
  description = "Elm version picker";

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

        lamdera = pkgs.stdenv.mkDerivation rec {
          pname = "lamdera";
          version = "1.4.0";

          src = pkgs.fetchurl {
            url = "https://static.lamdera.com/bin/lamdera-${version}-linux-x86_64";
            sha256 = "sha256-YofQhTI3qAa+c9QyEqLtg5vWl/KxuUZqjRNerkOFdcQ=";
          };

          dontUnpack = true;

          installPhase = ''
            install -Dm755 $src $out/bin/lamdera
          '';
        };
      in
      pkgs.stdenv.mkDerivation {
        pname = "elm";
        version = "0.19.3";
        nativeBuildInputs = [ lamdera ] ++ elmVersions;

        src = pkgs.writeText "elm" ''
          #!/usr/bin/env bash

          # A script that automatically runs the correct Elm version based on elm.json.
          # Assumes the following binaries in $PATH: 
          # lamdera
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
          if grep -q '"lamdera/' "$dir/elm.json"; then
            # if elm.json refers to Lamdera packages then run the Lamdera compiler.
            lamdera "$@"
          else 
            if grep -qE '"type"\s*:\s*"package"' "$dir/elm.json"; then
              # If elm.json specifies that this is a package then run the latest Elm compiler.
              elm-latest "$@"
            else
              # Otherwise this must be an application, so check which Elm version is specified.
              version="$(grep -P '\"elm-version\"\s*:\s*\"\d+\.\d+\.\d+\"' "$dir/elm.json" | cut -d '"' -f 4)"

              if test -z "$version"; then
                # If no version information is found in elm.json, fall back to using the latest Elm version.
                elm-latest "$@"
              else
                "elm-$version" "$@"
              fi
            fi
          fi'';

        dontUnpack = true;

        installPhase = ''
          mkdir -p $out/bin

          # Install the wrapper.
          install -Dm755 $src $out/bin/elm

          # Copy the Lamdera binary.
          cp "${lamdera}/bin/lamdera" "$out/bin/lamdera"

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
