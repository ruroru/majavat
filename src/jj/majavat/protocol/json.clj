(ns jj.majavat.protocol.json)

(defprotocol Json
  "Strategy for serializing a value to a JSON string for the `json` filter.

  Majavat ships a built-in implementation, but you can plug in your own
  (for example one backed by Jackson or Cheshire) by passing it as the
  `:json-serializer` key of the environment, e.g.

      (defrecord JacksonSerializer [mapper]
        Json
        (to-json [_ value _opts]
          (.writeValueAsString mapper value)))

  The `json` filter will then call your implementation instead of the
  built-in one."

  (to-json [this value opts]
    "Serializes value to a JSON string. opts is a map of options (may be nil);
     the built-in serializer honours {:indent n} for pretty-printing, custom
     implementations are free to ignore it."))
