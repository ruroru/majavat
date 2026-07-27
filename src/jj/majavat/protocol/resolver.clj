(ns jj.majavat.protocol.resolver)

(defprotocol TemplateResolver
  "Protocol for template resolution and access operations."

  (read-template [this content-path]
    "Returns the contents of the template at content-path as a string, or nil if not found.")

  (template-exists? [this content-path]
    "Returns true if the template exists at content-path, false otherwise."))
