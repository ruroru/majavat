# Majavat

A templating engine for Clojure

## Installation

Add majavat to dependency list

```clojure
[org.clojars.jj/majavat "1.6.0"]
```

## Usage

### Rendering templates

```clojure
(:require
  [jj.majavat :as majavat]
  [jj.majavat.renderer.sanitizer :refer [->Html]])

(majavat/render-file "index.html" {:user "jj"})
```

Additional options can be passed with

```clojure
(majavat/render-file "index.html" {:user "jj"} {:return-type :input-stream
                                                :sanitizer   (->Html)})
```

All supported options:

| Option              | Default Value      | Supported Options                     |
|---------------------|--------------------|---------------------------------------|
| `return-type`       | `:string`          | `:string`, `:input-stream`            |
| `template-resolver` | `ResourceResolver` | Any `TemplateResolver` implementation |
| `cache?`            | `true`             | `true`, `false`                       |
| `sanitizer`         | `nil`              | Any `Sanitizer` implementation        |

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

| filters       | type          | example                             |
|---------------|---------------|-------------------------------------|
| capitalize    | String        | "hello world" → "Hello world"       |
| lower-case    | String        | "HELLO WORLD" → "hello world"       |
| title-case    | String        | "hello world" → "Hello World"       |
| trim          | String        | "  hello  " → "hello"               |
| upper-case    | String        | "hello world" → "HELLO WORLD"       |
| upper-roman   | String        | "iv" → "IV"                         |
| long          | String        | "123" → 123L                        |
| int           | String        | "123" → 123                         |
| name          | keyword       | :name → "name"                      |
| inc           | Number        | 5 → 6                               |
| dec           | Number        | 5 → 4                               |
| file-size     | Number        | 2048 → "2 KB"                       |
| default "foo" | nil           | nil -> "foo"                        |
| date  "yyyy"  | LocalDate     | Instance of LocalDate -> "2025"     | 
| date  "yyyy"  | LocalDateTime | Instance of LocalDateTime -> "2025" | 

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

or

```
hello {% let foo = bar %}{{ foo.baz }}{% endlet %}
```

```clojure
(render-file "input-file" {:bar {:baz "baz"}}) ;; returns "hello baz"
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

#### csrf

CSRF token can be added via

```
{% csrf-token %}
```

and when rendering file `:csrf-token` has to be provided

```clojure
(render-file "input-file" {:csrf-token "foobarbaz"}) ;; returns <input type="hidden" name="csrf_token" value="foobarbaz"> 
```

#### Query string

input-file with content

```
/foo{% query-string foo %}
```

```clojure
(render-file "input-file" {:foo {:count 2}}) ;; returns "/foo?count=2"
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

## Sanitizer

`Sanitizer` protocol provides a way to sanitize and cleanup values.

### Usage

```clojure
(sanitize (->Html) "<foo>bar</baz>") ;; => &lt;foo&gt;bar&lt;/baz&gt;
```

### Built-in Implementations

- **Html** - implementation for html pages
- **Json** - implementation for Json

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
