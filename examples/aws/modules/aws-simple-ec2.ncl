let Aws = import "./types.ncl" in
let Utils = import "../utils.ncl" in
{
  aws.simple.ec2.instances
    | {
      _ | {
        ami | String,
        type | String,
        public_key_name | String,
        network,
        public_ip
      }
    }
    | Utils.Elaborate "public_ip" (fun name => config.resource.aws_instance."%{name}".public_ip)
    | default
    = {},

  config =
    aws.simple.ec2.instances
    |> std.record.fields
    |> std.array.map (fun name =>
      let cfg = aws.simple.ec2.instances."%{name}"
      in
      {
        resource.aws_security_group."%{name}_forwarder" = {
          vpc_id = cfg.network.vpc.id,
          ingress = [
            {
              from_port = 22,
              protocol = "tcp",
              to_port = 22,
              cidr_blocks = ["0.0.0.0/0"],

              description = "${null}",
              ipv6_cidr_blocks = [],
              prefix_list_ids = [],
              security_groups = [],
              self = false,
            }
          ],
          egress = [
            {
              from_port = 0,
              protocol = "-1",
              to_port = 0,
              cidr_blocks = ["0.0.0.0/0"],

              description = "${null}",
              ipv6_cidr_blocks = [],
              prefix_list_ids = [],
              security_groups = [],
              self = false,
            }
          ],
        },

        resource.aws_instance."%{name}" = {
          ami = cfg.ami,
          instance_type = cfg.type,
          subnet_id = cfg.network.subnet.id,
          vpc_security_group_ids = [config.resource.aws_security_group."%{name}_forwarder".id],
          key_name = cfg.public_key_name,
          associate_public_ip_address = true,
        },
      }
    )
    |> std.record.merge_all,
}
