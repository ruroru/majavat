(ns jj.majavat.resolver)

(defprotocol ContentResolver
  "Protocol for content resolution and access operations.

  Provides a uniform interface for resolving paths and accessing content
  from various sources (filesystem, classpath, URLs, etc.). Implementations
  should handle path normalization and provide consistent behavior across
  different content sources."

  (resolve-path [this base-path relative-path]
    "Resolve a relative path against a base path.

    Args:
      base-path     - The base path to resolve against (string)
      relative-path - The relative path to resolve (string)

    Returns:
      A resolved absolute path (string) that can be used with other
      ContentResolver methods.

    Example:
      (resolve-path resolver \"/base/path\" \"../file.txt\")
      ;; => \"/base/file.txt\"")

  (read-content [this content-path]
    "Read the contents of a resource at the specified path.

    Args:
      content-path - The path to the content resource (string)

    Returns:
      The content as a string if the resource exists, nil if the resource
      doesn't exist or cannot be read.")

  (content-exists? [this content-path]
    "Check if a resource exists at the specified path.

    Args:
      content-path

    Returns:
      true if the resource exists and is accessible, false otherwise."))