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

  TerraformReference = Array String,

  TerraformField | Dyn -> Dyn = fun ctr => [| 'Undefined, 'Literal ctr, 'Reference TerraformReference, 'ProviderComputed TerraformReference |],

  is_terraform_field | Dyn -> Bool = fun v =>
    std.is_enum v
    && (
      let tag = (std.enum.to_tag_and_arg v).tag in
      tag == "Undefined" || tag == "Literal" || tag == "Reference" || tag == "ProviderComputed"
    ),

  Tf = fun ctr =>
    std.contract.any_of [
      TerraformField ctr,
      std.contract.custom (fun label value => 'Ok ('Literal (std.contract.apply ctr label value)))
    ],

  resolve_reference : Array String -> String = fun ns => "${%{std.string.join "." ns}}",

  typeof : Dyn -> [| 'TerraformField, 'Array, 'Other |] = fun v =>
    if is_terraform_field v then
      'TerraformField
    else if std.is_array v then
      'Array
    else
      'Other,

  resolve_provider_computed =
    let rec
      resolve_field_action | Array String -> TerraformField Dyn -> Dyn = fun path =>
        match {
          'Undefined => 'Delete,
          'Literal value => 'KeepStop value,
          'Reference ref => 'KeepStop (resolve_reference ref),
          'ProviderComputed p if path == p => 'Delete,
          'ProviderComputed p => 'KeepStop (resolve_reference p),
        },
      go | Array String -> Dyn -> [| 'KeepStop Dyn, 'Continue Dyn, 'Delete |] = fun path field =>
        typeof field
        |> match {
          'TerraformField => resolve_field_action path field,
          'Array => 'KeepStop (std.array.map resolve_provider_computed field),
          'Other => 'Continue field,
        },
    in
    fun v =>
      if is_terraform_field v then
        resolve_field_action [] v
        |> match {
          'Delete => null,
          'KeepStop v => v,
        }
      else
        deep_filter_map go v,

  remove_empty_records =
    let rec
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
    in go,

  is_defined_terraform_field_record | Dyn -> Bool = fun x => is_terraform_field x && x != 'Undefined,

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
        acc
        |> match {
          [head] => { "%{head}" | force = 'ProviderComputed (prefix @ field.path), .. },
          [head, ..tail] => { "%{head}" = elaborated_record prefix tail field, .. },
        }
    }
    in fun prefix r field => candidates r [] field |> filter_mergeable r |> std.array.map (elaborate prefix),

  merge_elaborated | { _ : Dyn } -> ElaboratedField -> { _ : Dyn } = fun record field =>
    if mergeable record field then
      record & field.record
    else
      record,

  ComputedFields
    | Array String -> Array FieldDescriptor -> Dyn -> Dyn -> Dyn
    = fun prefix fields _l r =>
      fields
      |> std.array.map (elaborate_field prefix r)
      |> std.array.flatten
      |> std.array.fold_left merge_elaborated r,

  undefined = 'Undefined,
}
