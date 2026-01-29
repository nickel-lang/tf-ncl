{
  trace_val = fun x => std.trace (std.serialize 'Json x) x,

  Elaborate | String -> (String -> Dyn) -> Dyn -> { _ | Dyn } -> { _ | Dyn } = fun field elaborate _l r =>
    std.record.fields r |> std.array.fold_left (fun r name => r & { "%{name}"."%{field}" = elaborate name }) r,
}
