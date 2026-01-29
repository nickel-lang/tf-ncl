{
  Simple =
    { .. }
    & (import "./aws-availability-zones.ncl")
    & (import "./aws-provider.ncl")
    & (import "./aws-simple-vpc.ncl")
    & (import "./aws-simple-ec2.ncl"),

  S3Backend = import "./aws-s3-backend.ncl",
}
