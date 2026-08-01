{
  description = "Elm 0.19.2";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };

      stdenv.mkDerivation rec {
        name = "elm-${version}";

        version = "0.19.2";

        # https://nixos.wiki/wiki/Packaging/Binaries
        src = pkgs.fetchurl {
          url = "https://github.com/elm/compiler/releases/download/0.19.2/elm-${version}-linux-x64.gz";
          sha256 = "";
        };
        
        sourceRoot = ".";

        unpackPhase = ''
          cp $src elm.gz
          gzip -d elm.gz
        '';

        installPhase = ''
          install -m755 -D elm $out/bin/elm
        '';
      };
  };
}
