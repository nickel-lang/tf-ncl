{
  uniq | Array Dyn -> Array Dyn =
    let go : { visited : { _ : {} }, out : Array Dyn } -> Dyn -> { visited : { _ : {} }, out : Array Dyn } = fun acc nxt =>
      let as_str = std.serialize 'Json nxt in
      if std.record.has_field as_str acc.visited then
        acc
      else
        { visited = std.record.insert as_str {} acc.visited, out = acc.out @ [nxt] }
    in
    fun arr =>
      (std.array.fold_left go { visited = ({} | { _ : {} }), out = [] } (arr | Array Dyn)).out,

  collect_teams = fun users =>
    users
    |> std.record.values
    |> std.array.map (fun user => user.extra-teams)
    |> std.array.flatten
    |> uniq
    |> std.array.prepend "all",

  collect_users = fun users team =>
    users
    |> std.record.fields
    |> std.array.filter (fun user =>
      team == "all" || std.array.elem team users."%{user}".extra-teams
    ),

  mk_teams = fun config users =>
    users
    |> collect_teams
    |> std.array.map (fun team =>
      {
        resource.github_team."%{team}-team" = {
          name = team,
          privacy = "closed",
        },
        resource.github_team_members."%{team}-members" = {
          team_id = config.resource.github_team."%{team}-team".id,
          members =
            collect_users users team
            |> std.array.map (fun user =>
              {
                username = user,
                role =
                  if users."%{user}".is-admin then
                    "maintainer"
                  else
                    "member",
              }
            ),
        },
      }
    )
    |> std.record.merge_all,

  mk_memberships = fun users =>
    users
    |> std.record.fields
    |> std.array.map (fun name =>
      let user = users."%{name}" in
      {
        resource.github_membership."%{name}" = {
          role = if user.is-admin then "admin" else "member",
          username = name,
        }
      }
    )
    |> std.record.merge_all
}
