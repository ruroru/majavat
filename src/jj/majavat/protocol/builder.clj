(ns jj.majavat.protocol.builder)

(defprotocol Builder
  "Turns a template into a render function.

   A builder is created by `jj.majavat/build-renderer` through a constructor
   taking `[pre-render-context environment]`; a custom one can be supplied with
   the `:builder` option. `build-renderer` must return a function of one
   argument (the render context)."
  (build-renderer [this file-path template-resolver renderer escape-config error-handler]))
