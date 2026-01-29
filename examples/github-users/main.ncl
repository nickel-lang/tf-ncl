let Tf = import "./tf-ncl-schema.ncl" in
let User = import "./user-contract.ncl" in
let { TwoAdmins, .. } = import "./validation.ncl" in
let { mk_teams, mk_memberships, .. } = import "./lib.ncl" in
{
  Users = { _ : User },

  users | Users = import "./users.ncl",

  config =
    {
      terraform.required_providers = Tf.required_providers,
    }
    & mk_memberships users
    & mk_teams config users,
} | Tf.Config
