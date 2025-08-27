# Majavat

A templating engine for Clojure

## Installation

Add majavat to dependency list

```clojure
[org.clojars.jj/majavat "1.0.6"]
```

## Usage

### Rendering templates

```clojure
(:require
  [jj.majavat :as majavat])

(majavat/render-file "index.html" {:user "jj"})
```

Additional options can be passed with

```clojure
(majavat/render-file "index.html" {:user "jj"} {:return-type :input-stream})
```

All supported options:

| Option              | Default Value      | Supported Options                     |
|---------------------|--------------------|---------------------------------------|
| `return-type`       | `:string`          | `:string`, `:input-stream`            |
| `template-resolver` | `ResourceResolver` | Any `TemplateResolver` implementation |
| `cache?`            | `true`             | `true`, `false`                       |
| `character-escaper` | `nil`              | Any `CharEscaper` implementation      |

### Creating templates

#### Inserting value

Rendering `file.txt` with content

```
Hello {{ name }}!
```

```clojure
(render-file "file.txt" {:name "world"}) ;; => returns Hello world!
```

or with a filter

```
Hello {{ name | upper-case }}!
```

```clojure
(render-file "file.txt" {:name "world"}) ;; => returns Hello WORLD!
```

| supported filters | type    |
|-------------------|---------|
| capitalize        | String  |
| lower-case        | String  |
| title-case        | String  |
| trim              | String  |
| upper-case        | String  |
| upper-roman       | String  |
| name              | keyword |
| inc               | Number  |
| dec               | Number  |

#### Conditionals

Rendering input file with content:

```
"Hello {% if name %}{{name}}{% else %}world{% endif %}!"
```

```clojure
(render-file "input-file" {:name "jj"}) ;; returns "Hello jj!"
(render-file "input-file" {}) ;; returns "Hello world!"
```

or

```
"Hello {% if not name %}world{% else %}jj{% endif %}!"
```

```clojure
(render-file "input-file" {:name "foo"}) ;; returns "Hello jj!"
(render-file "input-file" {}) ;; returns "Hello world!"
```

#### Looping

Rendering input file with content:

```
{% for item in items %}
- {{ item }} is {{ loop.index }} of {{ loop.total }}
{% endfor %}
```

```clojure
(render-file "input-file" {:items ["Apple" "Banana" "Orange"]}) ;; returns "- Apple is 0 of 3\n- Banana is 1 of 3\n- Orange is 2 of 3"
```

The loop context provides access to:

`loop.total` - total number of items in the collection

`loop.index` - current 0-based index position

`loop.first?` - true only for the first item

`loop.last?` - true only for the last item

#### Including template

file.txt content

```
foo
```

Rendering input file with content:

```
included {% include "file.txt" %}
```

```clojure
(render-file "input-file" {}) ;; returns "included foo"
```

#### Setting value

You can set value within a template via:

```
hello {% let foo = "baz" %}{{ foo }}{% endlet %}
```

```clojure
(render-file "input-file" {}) ;; returns "hello baz"
```

#### Extending template

file.txt content

```
foo
{% block content %}
baz
```

Rendering input file with content:

```
{% extends content "file.txt" %}
bar
```

```clojure
(render-file "input-file" {}) ;; returns "foo\nbar\nbaz"
```

#### Comments

input-file with content
```
foo{# bar baz #}
```

```clojure
(render-file "input-file" {}) ;; returns "foo"
```


## TemplateResolver

The `TemplateResolver` protocol provides a uniform interface for accessing template content from different sources.

### Protocol Methods

#### `open-reader`

Returns reader for that template, or `nil` if not found.

```clojure
(open-reader "/templates/header.html")
```

#### `template-exists?`

Check if template exists at a path.

```clojure
(template-exists? resolver "/templates/footer.html") ;; => true
```

### Built-in Implementations

- **ResourceResolver** (default) - Reads from classpath
- **FsResolver** - Reads from filesystem

## License

Copyright © 2025 [ruroru](https://github.com/ruroru)

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
https://www.eclipse.org/legal/epl-2.0/.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
