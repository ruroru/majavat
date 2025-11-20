# Majavat

A templating engine for Clojure

## Installation

Add majavat to dependency list

```clojure
[org.clojars.jj/majavat "1.12.6"]
```

## Usage

### Rendering templates

```clojure
(:require
  [jj.majavat :as majavat]
  [jj.majavat.renderer.sanitizer :refer [->Html]])

(def render-fn (majavat/build-renderer "index.html"))

(render-fn {:user "jj"})
```

Additional options can be passed with

```clojure
(def render-fn (majavat/build-renderer "index.html" {:cache?     false
                                                     :pre-render {:key "value"}
                                                     :renderer   (->StringRenderer {:sanitizer (->Html)})}))

(render-fn {:user "jj"})
```

All supported options:

| Option              | Default Value                           | Supported Options                                   |
|---------------------|-----------------------------------------|-----------------------------------------------------|
| `renderer`          | [`StringRenderer`](#renderer-protocol)  | Any [`Renderer`](#renderer-protocol) implementation |
| `cache?`            | `true`                                  | `true`, `false`                                     |
| `template-resolver` | [`ResourceResolver`](#templateresolver) | [`TemplateResolver`](#templateresolver)             |
| `pre-render`        | {}                                      | Map                                                 |

### Creating templates

#### Inserting value

Rendering `file.txt` with content

```
Hello {{ name }}!
```

```clojure
(def render-fn (build-renderer "file.txt"))
(render-fn {:name "world"}) ;; => returns Hello world!
```

or with a filter

```
Hello {{ name | upper-case }}!
```

```clojure
(def render-fn (build-renderer "file.txt"))

(render-fn {:name "world"}) ;; => returns Hello WORLD!
```

| filters                   | type          | example                               |
|---------------------------|---------------|---------------------------------------|
| capitalize                | String        | "hello world" → "Hello world"         |
| lower-case                | String        | "HELLO WORLD" → "hello world"         |
| title-case                | String        | "hello world" → "Hello World"         |
| trim                      | String        | "  hello  " → "hello"                 |
| upper-case                | String        | "hello world" → "HELLO WORLD"         |
| upper-roman               | String        | "iv" → "IV"                           |
| long                      | String        | "123" → 123L                          |
| int                       | String        | "123" → 123                           |
| name                      | keyword       | :name → "name"                        |
| inc                       | Number        | 5 → 6                                 |
| dec                       | Number        | 5 → 4                                 |
| file-size                 | Number        | 2048 → "2 KB"                         |
| default "foo"             | nil           | nil -> "foo"                          |
| date  "yyyy"              | LocalDate     | Instance of LocalDate -> "2025"       | 
| date  "yyyy"              | LocalDateTime | Instance of LocalDateTime -> "2025"   | 
| date "hh/mm"              | LocalTime     | Instance of LocalTime ->  "11/11"     |
| date "hh/mm" "Asia/Tokyo" | ZonedDateTime | Instance of ZonedDateTime ->  "11/11" |
| date "hh/mm" "Asia/Tokyo" | Instant       | Instance of Instant ->  "11/11"       |

#### Conditionals

Rendering input file with content:

```
"Hello {% if name %}{{name}}{% else %}world{% endif %}!"
```

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {:name "jj"}) ;; returns "Hello jj!"
(render-fn {}) ;; returns "Hello world!"
```

or

```
"Hello {% if not name %}world{% else %}jj{% endif %}!"
```

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {:name "foo"}) ;; returns "Hello jj!"
(render-fn {}) ;; returns "Hello world!"
```

#### Looping

Rendering input file with content:

```
{% for item in items %}
- {{ item }} is {{ loop.index }} of {{ loop.total }}
{% endfor %}
```

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {:items ["Apple" "Banana" "Orange"]}) ;; returns "- Apple is 0 of 3\n- Banana is 1 of 3\n- Orange is 2 of 3"
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
(def render-fn (build-renderer "input-file"))

(render-fn {}) ;; returns "included foo"
```

#### Setting value

You can set value within a template via:

```
hello {% let foo = "baz" %}{{ foo }}{% endlet %}
```

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {}) ;; returns "hello baz"
```

or

```
hello {% let foo = bar %}{{ foo.baz }}{% endlet %}
```

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {:bar {:baz "baz"}}) ;; returns "hello baz"
```

#### Extending template

file.txt content

```
foo
{% block %}
baz
```

Rendering input file with content:

```
{% extends "file.txt" %}
bar
```

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {}) ;; returns "foo\nbar\nbaz"
```

#### Comments

input-file with content

```
foo{# bar baz #}
```

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {}) ;; returns "foo"
```

#### csrf

CSRF token can be added via

```
{% csrf-token %}
```

and when rendering file `:csrf-token` has to be provided

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {:csrf-token "foobarbaz"}) ;; returns <input type="hidden" name="csrf_token" value="foobarbaz"> 
```

#### Query string

input-file with content

```
/foo{% query-string foo %}
```

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {:foo {:count 2}}) ;; returns "/foo?count=2"
```

#### Now

input-file with content

```
default format {% now %}
formatted {% now "yyyy-MM-dd" %}
formatted with tz {% now "yyyy-MM-dd hh:mm " "Asia/Tokyo" %}
```

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {}) ;; returns "default format 2011/11/11 11:11\nformatted 2011-11-11\ntormatted with tz 2011-11-11 23:11"
```

### Verbatim

input-file with content

```
{% verbatim %}foo{{bar}}{%baz%}{#qux#}quux{% endverbatim %}
```

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {}) ;; returns "foo{{bar}}{%baz%}{#qux#}quux"
```

## Renderer Protocol

### Configuration

Supported options:

**:sanitizer** - Input sanitization strategy

### render

Renders a template using the provided context.

- template - template AST
- context - Map of variables for template interpolation

**Returns** - Rendered output

### Built-in Implementations

#### StringRenderer

Returns rendered output as a String
clojure

```clojure
(->StringRenderer {:sanitizer (->Html)})
```

#### InputStreamRenderer

Returns rendered output as an InputStream for streaming large content

```clojure
(->InputStreamRenderer {:sanitizer (->Json)})
```

#### PartialRenderer

Returns a partially rendered AST.

```clojure
(->PartialRenderer {})
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

## Available Extensions

- [File Renderer](https://github.com/ruroru/majavat-file-renderer) - Renders output directly to file.
- [TTL Builder](https://github.com/ruroru/majavat-ttl-builder) - Reloads cache on a scheduled interval.

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
