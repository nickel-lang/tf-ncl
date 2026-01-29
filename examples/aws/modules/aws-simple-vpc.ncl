let Aws = import "./types.ncl" in
let Utils = import "../utils.ncl" in
{
  aws.region | Aws.Region,

  aws.simple.networks
    | {
      _ | {
        region
          | Aws.Region
          | default
          = aws.region,
        cidr_block | Aws.CidrBlock,
        vpc,
        subnet,
      }
    }
    | Utils.Elaborate "vpc" (fun name => config.resource.aws_vpc."%{name}_vpc")
    | Utils.Elaborate "subnet" (fun name => config.resource.aws_subnet."%{name}")
    | default
    = {},

  aws.availability_zones =
    aws.simple.networks
    |> std.record.fields
    |> std.array.map (fun name =>
      let cfg = aws.simple.networks."%{name}"
      in
      {
        "%{name}_availability_zone" = {
          region = cfg.region,
        }
      }
    )
    |> std.record.merge_all,

  config =
    aws.simple.networks
    |> std.record.fields
    |> std.array.map (fun name =>
      let cfg = aws.simple.networks."%{name}"
      in
      {
        resource.aws_vpc."%{name}_vpc" = {
          cidr_block = "%{cfg.cidr_block.prefix}/%{std.string.from cfg.cidr_block.length}",
          tags."Name" = "Tf-Ncl %{name} VPC",
        },

        resource.aws_subnet."%{name}" = {
          cidr_block = "%{cfg.cidr_block.prefix}/%{std.string.from cfg.cidr_block.length}",
          vpc_id = config.resource.aws_vpc."%{name}_vpc".id,
          availability_zone = "${%{aws.availability_zones."%{name}_availability_zone".path}.names[0]}",
          tags."Name" = "Tf-Ncl %{name} Subnet",
        },

        resource.aws_internet_gateway."%{name}_gw" = {
          vpc_id = config.resource.aws_vpc."%{name}_vpc".id,
          tags."Name" = "Tf-Ncl %{name} Gateway",
        },

        resource.aws_route_table."%{name}_default_rt" = {
          vpc_id = config.resource.aws_vpc."%{name}_vpc".id,
          tags."Name" = "Tf-Ncl %{name} Internet Route Table",
        },

        resource.aws_route."%{name}_default_route" = {
          route_table_id = config.resource.aws_route_table."%{name}_default_rt".id,
          destination_cidr_block = "0.0.0.0/0",
          gateway_id = config.resource.aws_internet_gateway."%{name}_gw".id,
        },

        resource.aws_route."%{name}_default6_route" = {
          route_table_id = config.resource.aws_route_table."%{name}_default_rt".id,
          destination_ipv6_cidr_block = "::/0",
          gateway_id = config.resource.aws_internet_gateway."%{name}_gw".id,
        },

        resource.aws_route_table_association."%{name}_public_subnet_association" = {
          subnet_id = config.resource.aws_subnet."%{name}".id,
          route_table_id = config.resource.aws_route_table."%{name}_default_rt".id,
        },
      }
    )
    |> std.record.merge_all,
}
