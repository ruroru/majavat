(ns jj.majavat.protocol.yaml)

(defprotocol Yaml
  "Strategy for serializing a value to a YAML string for the `yaml` filter.

  Majavat ships a built-in implementation, but you can plug in your own
  (for example one backed by SnakeYAML or clj-yaml) by passing it as the
  `:yaml-serializer` key of the environment, e.g.

      (defrecord SnakeYamlSerializer [yaml]
        Yaml
        (to-yaml [_ value _opts]
          (.dump yaml value)))

  The `yaml` filter will then call your implementation instead of the
  built-in one."

  (to-yaml [this value opts]
    "Serializes value to a YAML string. opts is a map of options (may be nil);
     the built-in serializer honours {:indent n} for the indentation width,
     custom implementations are free to ignore it."))
