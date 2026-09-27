{
  description = "Elm 0.19.2";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
        let 
          releases = [
            {version = "0.19.0"; name = "binary-for-linux-64-bit"; sha = "sha256-01mtvuiYI8ZBzaMmk4cI1yJ9x5qh8WLg2P4nXxgvUoo=";}
            {version = "0.19.1"; name = "binary-for-linux-64-bit"; sha = "sha256-5Er1K7J/clqXNHjlidmQpkKOEV/huxTwODMTTWwPFVw=";}
            {version = "0.19.2"; name = "elm-0.19.2-linux-x64"; sha = "sha256-ZjINJ3AWVPoRvQ6NhL35gpaU1XcMjc7i3t5hYPrVhzc=";}
          ];

          f = release: 
            stdenv.mkDerivation rec {
              version = release.version;
              
              name = "elm-${version}";
              
              src = 
                pkgs.fetchurl {
                  url = "https://github.com/elm/compiler/releases/download/${version}/${release.name}.gz";
                  sha256 = release.sha;
                };
              
              sourceRoot = ".";

              unpackPhase = ''
                cp $src $name.gz
                gzip -d $name.gz
              '';

              installPhase = ''
                install -m755 -D $name $out/bin/$name
              '';
            };
        in
        lib.lists.map f releases;
  };
}
