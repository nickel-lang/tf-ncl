[
  { uses = "actions/checkout@v4" },
  {
    uses = "cachix/install-nix-action@v31",
    name = "Installing Nix",
    with = {
      extra_nix_config = "experimental-features = nix-command flakes",
      nix_path = "nixpkgs=channel:nixos-unstable"
    }
  },
  {
    name = "Setup Cachix",
    with = {
      authToken = "${{ secrets.CACHIX_TWEAG_NICKEL_AUTH_TOKEN }}",
      name = "tweag-nickel"
    },
    uses = "cachix/cachix-action@v12"
  }
]
