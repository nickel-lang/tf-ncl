let Tf = import "./tf-ncl-schema.ncl" in
{
  users = [
    "alice",
    "bob",
    "charlie"
  ],

  memberships =
    users
    |> std.array.map (fun user =>
      {
        resource.github_membership."%{user}-membership" = {
          username = user,
          role = "member",
        }
      }
    )
    |> std.record.merge_all,

  config =
    {
      provider.github = [
        {
          token = "<placeholder-token>",
          owner = "<placeholder-organization>",
        }
      ],
    }
    & memberships,
} | Tf.Config
