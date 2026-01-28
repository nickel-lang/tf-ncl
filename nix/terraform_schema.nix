providers:
{ lib, runCommand, formats, terraform, cacert, schema-merge }:
let
  required_providers = providers: lib.mapAttrs
    (_: p: {
      inherit (p) version;
      source = lib.toLower p.provider-source-address;
    })
    providers;

  retrieveProviderSchema = name: provider:
    let
      cleanName = builtins.replaceStrings [ "_" ] [ "-" ] name;
      mainJson = (formats.json { }).generate "main.tf.json" {
        terraform.required_providers = required_providers { "${cleanName}" = provider; };
      };

      terraform-with-plugins = terraform.withPlugins (_: [ provider ]);
    in
    runCommand "${name}.json" { } ''
      cp ${mainJson} main.tf.json
      ${terraform-with-plugins}/bin/terraform init
      ${terraform-with-plugins}//bin/terraform providers schema -json >$out
    '';

  providersJson = (formats.json { }).generate "providers.json" (required_providers providers);
in
runCommand "schemas" { } ''
  mkdir schemas
  ${lib.concatStringsSep "\n" (lib.mapAttrsToList
    (name: provider: ''
      ln -s ${retrieveProviderSchema name provider} schemas/"${name}.json"
    '')
    providers)}
  ln -s ${providersJson} providers.json

  mkdir -p $out
  ln -s ${providersJson} $out/providers.json
  ${schema-merge}/bin/schema-merge . > $out/schema.json
''
