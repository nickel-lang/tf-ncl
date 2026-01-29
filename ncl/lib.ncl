{
  deep_filter_map
    : (Array String -> Dyn -> [| 'KeepStop Dyn, 'Continue Dyn, 'Delete |]) -> { _ : Dyn } -> { _ : Dyn }
    | doc m%"
      A kind of filter-map that processes records recursively.

      In `deep_filter_map f record`, `f` is a function that takes a record path and the value at that
      record path, and decides whether to

      - keep the value (possibly transforming it);
      - keep the value (possibly transforming it) and then, if the transformed
        value is a record, recurse into it; or
      - discard the value.

      # Examples

      Here we iterate through records, ignoring all booleans and incrementing all numbers.
      ```nickel
      let f = fun _path value => std.typeof value |> match {
          'Bool => 'Delete,
          'Number => 'KeepStop (value + 1),
          _ => 'Continue value,
        }
      in
      deep_filter_map f { foo = { bar = 1, baz = true }, quux = 2 }

      # => { foo = { bar = 2 }, quux = 3 }
      ```
    "%
    =
      let rec
        go
          : Array String -> (Array String -> Dyn -> [| 'KeepStop Dyn, 'Continue Dyn, 'Delete |]) -> { _ : Dyn } -> { _ : Dyn } -> String -> { _ : Dyn }
          = fun prefix f r acc field_name =>
            let prefix = prefix @ [field_name] in
            f prefix (r."%{field_name}")
            |> match {
              'KeepStop value => std.record.insert field_name value acc,
              'Continue value =>
                let new_value =
                  if std.is_record value then
                    (deep_filter_map_prefix prefix f (value | { _ : Dyn })) | Dyn
                  else
                    value
                in
                std.record.insert field_name new_value acc,
              'Delete => acc,
            },
        deep_filter_map_prefix
          : Array String -> (Array String -> Dyn -> [| 'KeepStop Dyn, 'Continue Dyn, 'Delete |]) -> { _ : Dyn } -> { _ : Dyn }
          = fun prefix f r =>
            r
            |> std.record.fields
            |> std.array.fold_left (go prefix f r) {},
      in
      deep_filter_map_prefix [],

  TaggedUnion = fun tag_field tags shapes label value =>
    let value =
      std.contract.apply { "%{tag_field}" | tags, .. } label value
    in std.contract.apply ({ "%{tag_field}" } & shapes value."%{tag_field}") label value,

  TerraformReference = Array String,

  TerraformField = fun ctr =>
    TaggedUnion
      "terraform_field_type"
      [| 'Undefined, 'Literal, 'Reference, 'ProviderComputed |]
      (match {
        'Undefined => {},
        'Literal => { value | ctr },
        'Reference => { value | Array Dyn },
        'ProviderComputed => { path | TerraformReference },
      }
      ),

  is_terraform_field_record : Dyn -> Bool = fun v =>
    std.is_record v && std.record.has_field "terraform_field_type" (v | { _ : Dyn }),

  Tf = fun ctr label value_ =>
    if is_terraform_field_record value_ then
      std.contract.apply (TerraformField ctr) label value_
    else
      { terraform_field_type = 'Literal, value | (fun _l v => std.contract.apply ctr label v) = value_ },

  provider_computed = fun path_ =>
    { terraform_field_type = 'ProviderComputed, path = path_ },

  undefined = { terraform_field_type | default = 'Undefined },

  resolve_reference : Array String -> String = fun ns => "${%{std.string.join "." ns}}",

  typeof : Dyn -> [| 'TerraformField, 'Array, 'Other |] = fun v =>
    if is_terraform_field_record v then
      'TerraformField
    else if std.is_array v then
      'Array
    else
      'Other,

  resolve_provider_computed =
    let { go, resolve_field_action, .. } = {
      resolve_field_action | Array String -> TerraformField Dyn -> Dyn = fun path field =>
        field.terraform_field_type
        |> match {
          'Undefined => 'Delete,
          'Literal => 'KeepStop field.value,
          'Reference => 'KeepStop (resolve_reference field.value),
          'ProviderComputed =>
            if path == field.path then
              'Delete
            else
              'KeepStop (resolve_reference field.path)
        },
      go | Array String -> Dyn -> [| 'KeepStop Dyn, 'Continue Dyn, 'Delete |] = fun path field =>
        typeof field
        |> match {
          'TerraformField => resolve_field_action path field,
          'Array => 'KeepStop (std.array.map resolve_provider_computed field),
          'Other => 'Continue field,
        },
    }
    in
    fun v =>
      if is_terraform_field_record v then
        (resolve_field_action [] v).value
      else
        deep_filter_map go v,

  remove_empty_records =
    let { go, .. } = {
      go_field : { _ : Dyn } -> String -> { _ : Dyn } = fun r f =>
        if !std.is_record r."%{f}" then
          r
        else
          let new_field = go (r."%{f}" | { _ : Dyn }) in
          if new_field == {} then
            std.record.remove f r
          else
            std.record.update f (new_field | Dyn) r,
      go : { _ : Dyn } -> { _ : Dyn } = fun r => r |> std.record.fields |> std.array.fold_left go_field r,
    }
    in go,

  is_defined_terraform_field_record | Dyn -> Bool = fun x => is_terraform_field_record x && x.terraform_field_type != 'Undefined,

  has_defined_field | String -> { _ : Dyn } -> Bool = fun name r => std.record.has_field name r && is_defined_terraform_field_record r."%{name}",

  has_defined_field_path | Array String -> { _ : Dyn } -> Bool = fun path r =>
    if std.array.length path == 1 then
      has_defined_field (std.array.first path) r
    else
      let field =
        std.array.first path
      in
      std.record.has_field field r
      && std.is_record r."%{field}"
      && has_defined_field_path (std.array.drop_first path) r."%{field}",

  FieldDescriptor = {
    prio | [| 'Default, 'Force |],
    path | Array String,
    ..
  },

  ElaboratedField = {
    prio | [| 'Default, 'Force |],
    path | Array String,
    record | { _ : Dyn },
  },

  mergeable | { _ : Dyn } -> FieldDescriptor -> Bool = fun r field => !(has_defined_field_path field.path r && field.prio == 'Default),

  elaborate_field | Array String -> { _ : Dyn } -> FieldDescriptor -> Array ElaboratedField =
    let { candidates, filter_mergeable, elaborate, .. } = {
      candidates | { _ : Dyn } -> Array String -> FieldDescriptor -> Array FieldDescriptor = fun r prefix field =>
        let head = std.array.first field.path in
        let tail = std.array.drop_first field.path in
        if head == "_" then
          std.record.fields r
          |> std.array.map (fun name =>
            candidates
              r
              prefix
              {
                prio = field.prio,
                path = [name] @ tail
              }
          )
          |> std.array.flatten
        else if std.array.length field.path == 1 then
          [
            {
              prio = field.prio,
              path = prefix @ [head]
            }
          ]
        else
          let r' =
            if std.record.has_field head r then r."%{head}" else {}
          in candidates r' (prefix @ [head]) { prio = field.prio, path = tail },

      filter_mergeable | { _ : Dyn } -> Array FieldDescriptor -> Array FieldDescriptor = fun r => std.array.filter (fun field => mergeable r field),

      elaborate | Array String -> FieldDescriptor -> ElaboratedField = fun prefix field =>
        {
          prio = field.prio,
          path = field.path,
          record = elaborated_record prefix field.path field
        },

      elaborated_record | Array String -> Array String -> FieldDescriptor -> { _ : Dyn } = fun prefix acc field =>
        let head = std.array.first acc in
        if std.array.length acc > 1 then
          { "%{head}" = elaborated_record prefix (std.array.drop_first acc) field, .. }
        else
          { "%{head}" | force = provider_computed (prefix @ field.path), .. },
    }
    in fun prefix r field => candidates r [] field |> filter_mergeable r |> std.array.map (elaborate prefix),

  merge_elaborated | { _ : Dyn } -> ElaboratedField -> { _ : Dyn } = fun record field =>
    if mergeable record field then
      record & field.record
    else
      record,

  ComputedFields | Array String -> Array FieldDescriptor -> Dyn -> Dyn -> Dyn = fun prefix fields _l r => fields |> std.array.map (elaborate_field prefix r) |> std.array.flatten |> std.array.fold_left merge_elaborated r,
}
