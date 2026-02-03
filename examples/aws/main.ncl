let Tf = import "./tf-ncl-schema.ncl" in
let Aws = import "./modules/aws.ncl" in
let ami-id = match {
  'eu-central-1 => "ami-0f1cf34dcb4057a5f", # Ubuntu 22.10
}
in
let private-key-path = "./<placeholder-private-key>" in
(
  {
    aws.region = 'eu-central-1,

    aws.state = {
      bucket = "tf-ncl-aws-example",
      key = "aws-demo.tfstate",
      region = aws.region,
    },

    aws.credentials.assume_role.role_arn = "placeholder_iam_role",

    aws.simple.networks.test = {
      cidr_block = {
        prefix = "10.1.0.0",
        length = 16,
      },
    },

    aws.simple.ec2.instances = {
      test = {
        ami = ami-id aws.region,
        type = "t2.micro",
        network = aws.simple.networks.test,
        public_key_name = "${resource.aws_key_pair.ssh_key.key_name}",
      },
    },

    config = {
      data.external.ssh_key = {
        program = ["./ssh-pubkey.sh", private-key-path],
      },

      resource.aws_key_pair.ssh_key = {
        public_key = "${data.external.ssh_key.result.public_key}",
      },

      output."instance_ip".value = aws.simple.ec2.instances.test.public_ip,
    },
  }
    | Aws.Simple
    | Aws.S3Backend
) | Tf.Config
