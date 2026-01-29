let Tf = import "./tf-ncl-schema.ncl" in
{
  config.resource.null_resource.hello-world = {
    provisioner.local-exec = [
      {
        command = m%"
        echo 'Hello, world!'
      "%
      }
    ],
  },
} | Tf.Config
