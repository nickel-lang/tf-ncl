let name_of_config = fun config =>
  config
  |> std.record.values
  |> std.array.filter (std.is_string)
  |> std.string.join ", "
in
let sanitize = fun x =>
  std.function.pipe
    x
    [
      std.string.replace_regex "[^A-Za-z0-9-_]" "-",
      std.string.lowercase
    ]
in
{
  configs
    | Array { .. }
    | not_exported,
  matrix
    | not_exported
    | {
      name | String,
      job
        | {
          steps | Array { .. },
          ..
        },
      ..
    },
  jobs =
    configs
    |> std.array.map (fun config' =>
      {
        "%{sanitize "%{matrix.name}-%{name_of_config config'}"}" =
          (
            matrix & { config = config' }
          ).job
          & {
            name = "%{matrix.name} / (%{name_of_config config'})",
          }
      }
    )
    |> std.record.merge_all
}
