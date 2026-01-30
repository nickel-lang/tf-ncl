let Aws = import "./types.ncl" in
{
  aws.state
    | {
      bucket | String,
      key | String,
      region | Aws.Region,
    },

  config.terraform.backend.s3 = {
    bucket = aws.state.bucket,
    key = aws.state.key,
    region = std.string.from aws.state.region,
  },
}
