let User = import "./user-contract.ncl" in
{
  TwoAdmins = fun label value =>
    let value = std.contract.apply { _ : User } label value in
    let admins = value |> std.record.values |> std.array.filter (fun u => u.is-admin) in
    if std.array.length admins < 2 then
      std.contract.blame_with "We need at least 2 admins" label
    else
      value,

  NoA = fun label value =>
    let value = std.contract.apply { _ : Dyn } label value in
    let fields = std.record.fields value in
    if std.array.any (std.string.contains "a") fields then
      std.contract.blame_with "a is not allowed in usernames" label
    else
      value,
}
