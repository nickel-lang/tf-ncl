{
  inputs = {
    tf-ncl.url = "github:tweag/tf-ncl";
    utils.url = "github:numtide/flake-utils";
  };

  nixConfig = {
    extra-substituters = [ "https://tweag-nickel.cachix.org" ];
    extra-trusted-public-keys = [ "tweag-nickel.cachix.org-1:GIthuiK4LRgnW64ALYEoioVUQBWs0jexyoYVeLDBwRA=" ];
  };

  outputs = inputs: inputs.utils.lib.eachDefaultSystem (system:
    {
      devShell = inputs.tf-ncl.lib.${system}.mkDevShell {
        providers = p: {
          inherit (p) integrations_github;
        };
      };
    });
}
