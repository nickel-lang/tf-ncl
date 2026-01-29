{
  name = "Release",
  on.release.types = ["created"],
  on.workflow_dispatch = {
    inputs.release_tag.description = "The release tag to target"
  },
  jobs = {
    "tarball" = {
      name = "Upload Release Tarball",
      steps =
        (import "../../ncl/github/setup-steps.ncl")
        @ [
          {
            name = "Create Release Tarball",
            run = "nix build .#release --print-build-logs",
          },
          {
            name = "Upload Release Tarball",
            run = m%"
              gh release upload --clobber $RELEASE_TAG ./result/tf-ncl.tar.xz#"Generated Tf-Ncl Schemas"
            "%,
            env = {
              RELEASE_TAG = "${{ github.event_name == 'release' && github.event.release.tag_name || github.event.inputs.release_tag }}",
              GH_TOKEN = "${{ github.token }}",
            },
          }
        ],
      runs-on = "ubuntu-latest",
    }
  },
}
