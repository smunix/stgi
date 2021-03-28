{
  description = "A very basic flake";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils/master";
  }; 
  outputs =
    { self, nixpkgs, flake-utils,
      ...
    }:
    with flake-utils.lib;
    with nixpkgs.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let version = "${substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
          overlay = self: super:
            with self;
            with haskell.lib;
            with haskellPackages.extend(self: super: {
              tasty-html = unmarkBroken super.tasty-html;
              tasty-rerun = unmarkBroken super.tasty-rerun;
            });
            {
              stgi = rec {
                package = overrideCabal (callCabal2nix "stgi" ./. {}) (o: { version = "${o.version}-${version}"; });
                apps = {
                  stgi-exe = mkApp { drv = package; exePath = "/bin/stgi-exe"; };
                };
              };
            };
          overlays = [ overlay ];
      in
        with (import nixpkgs { inherit system overlays; });
        rec {
          packages = flattenTree (recurseIntoAttrs { stgi = stgi.package; });
          defaultPackage = packages.stgi;
          inherit (stgi) apps;
          defaultApp = apps.stgi-exe;
        });
}
