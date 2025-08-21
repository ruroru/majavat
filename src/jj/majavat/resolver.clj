(ns jj.majavat.resolver)

(defprotocol TemplateResolver
  "Protocol for template resolution and access operations."

  (open-reader [this content-path]
    "Returns a java.io.Reader for the template at content-path, or nil if not found.")

  (template-exists? [this content-path]
    "Returns true if the template exists at content-path, false otherwise."))