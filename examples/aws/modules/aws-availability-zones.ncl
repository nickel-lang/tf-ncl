let Utils = import "../utils.ncl" in
let Aws = import "./types.ncl" in
{
  aws.availability_zones
    | {
      _ | {
        region | Aws.Region,
        data,
        path,
      }
    }
    | Utils.Elaborate "data" (fun name => config.data.aws_availability_zones."%{name}")
    | Utils.Elaborate "path" (fun name => "data.aws_availability_zones.%{name}"),

  config =
    aws.availability_zones
    |> std.record.fields
    |> std.array.map (fun name =>
      {
        data.aws_availability_zones."%{name}" = {
          state = "available",
        },
      }
    )
    |> std.record.merge_all,
}
