# Majavat

A templating engine for Clojure

## Installation

Add majavat to dependency list

```clojure
[org.clojars.jj/majavat "1.18.0"]
```

## Usage

### Rendering templates

```clojure
(:require
  [jj.majavat :as majavat]
  [jj.majavat.renderer.sanitizer :refer [->Html]])

(def render-fn (majavat/build-renderer "index.html"))
;; or build html renderer, which will sanitize input
(def html-render-fn (majavat/build-html-renderer "index.html"))

(render-fn {:user "jj"})
(html-render-fn {:user "jj"})
```

Additional options can be passed with

```clojure
(def render-fn (majavat/build-renderer "index.html" {:cache?     false
                                                     :pre-render {:key "value"}
                                                     :filters    {:reverse (fn [value args]
                                                                             (string/reverse value))}
                                                     :renderer   (->StringRenderer)}))

(render-fn {:user "jj"})
```

All supported options:

| Option              | Default Value                           | Supported Options                                   |
|---------------------|-----------------------------------------|-----------------------------------------------------|
| `renderer`          | [`StringRenderer`](#renderer-protocol)  | Any [`Renderer`](#renderer-protocol) implementation |
| `cache?`            | `true`                                  | `true`, `false`                                     |
| `template-resolver` | [`ResourceResolver`](#templateresolver) | [`TemplateResolver`](#templateresolver)             |
| `pre-render`        | {}                                      | Map                                                 |
| `filters`           | {}                                      | Map                                                 |
| `sanitizer`         | nil                                     | Any Sanitizer implementation                        |
| `sanitizers`        | {}                                      | Keyword -> Sanitizer Map                            |

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

##### Built In Filters

| Filter                    | Type          | Example                                        |
|---------------------------|---------------|------------------------------------------------|
| append                    | String        | "hello" \| append " world" → "hello world"     |
| capitalize                | String        | "hello world" → "Hello world"                  |
| int                       | String        | "123" → 123                                    |
| long                      | String        | "123" → 123L                                   |
| lower-case                | String        | "HELLO WORLD" → "hello world"                  |
| prepend                   | String        | "hello" \| prepend " world" → "world hello"    |                                      
| slugify                   | String        | "Foo Bar" → "foo-bar"                          |
| title-case                | String        | "hello world" → "Hello World"                  |
| trim                      | String        | "  hello  " → "hello"                          |
| upper-case                | String        | "hello world" → "HELLO WORLD"                  |
| upper-roman               | String        | "iv" → "IV"                                    |
| name                      | Keyword       | :name → "name"                                 |
| abs                       | Number        | -1 → 1.0                                       |
| ceil                      | Number        | 1.99 → 2                                       |
| dec                       | Number        | 5 → 4                                          |
| file-size                 | Number        | 2048 → "2 KB"                                  |
| floor                     | Number        | 1.4 → 1.0                                      |
| inc                       | Number        | 5 → 6                                          |
| round                     | Number        | 1.99 → 2                                       |
| first                     | Sequential    | (list :foo :bar :baz) -> :foo                  |
| rest                      | Sequential    | (list :foo :bar :baz) -> (list :bar :baz)      |
| first                     | Map           | {:foo :a :bar :b :baz :c} -> [:foo :a]         |
| rest                      | Map           | {:foo :a :bar :b :baz :c} -> {:bar :b :baz :c} |
| default "foo"             | nil           | nil → "foo"                                    |
| date "yyyy"               | LocalDate     | Instance of LocalDate → "2025"                 | 
| date "yyyy"               | LocalDateTime | Instance of LocalDateTime → "2025"             | 
| date "hh/mm"              | LocalTime     | Instance of LocalTime →  "11/11"               |
| date "hh/mm" "Asia/Tokyo" | Instant       | Instance of Instant →  "11/11"                 |
| date "hh/mm" "Asia/Tokyo" | ZonedDateTime | Instance of ZonedDateTime →  "11/11"           |

##### User Provided filters Filters

Assoc :filter to option map, when building renderer, with this value

```clojure
{:quote (fn [value args]
          (format "\"%s\" - %s" value (first args)))}
```

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

##### for

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

##### each

In situations where loop context is not needed, `each` can be used

```
{% each item in items %}
- {{ item }}
{% endeach %}
```

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {:items ["Apple" "Banana" "Orange"]}) ;; returns "- Apple\n- Banana\n- Orange"
```

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

### Escape

If needed, ```Sanitizer``` implementation can be set/overridden via ```escape``` tag.

```
{% escape html %}foo{{bar}}{% endescape %}
```

```clojure
(def render-fn (build-renderer "input-file"))

(render-fn {:bar "<div/>"}) ;; returns "&lt;div/&gt;"
```
Available values:
* none
* html
* json

or ones provided by :sanitizers 

## RenderTarget Protocol

### render

Renders a template using the provided context.

- template - template AST
- context - Map of variables for template interpolation
- sanitizer - A record that implements `Sanitizer` protocol

**Returns** - Rendered output

### Built-in Implementations

#### StringRenderer

Returns rendered output as a String
clojure

```clojure
(->StringRenderer)
```

#### InputStreamRenderer

Returns rendered output as an InputStream for streaming large content

```clojure
(->InputStreamRenderer)
```

#### PartialRenderer

Returns a partially rendered AST.

```clojure
(->PartialRenderer)
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
- **None** - Implementation that does not sanitize

## Performance

Stress test was conducted rendering template 1000000 times using a standard web page with navigation, conditionals,
loops, and nested data access.

| Engine                | Total Time | Per Render | Throughput | vs Majavat    |
|-----------------------|------------|------------|------------|---------------|
| Majavat (String)      | 4.1s       | 4.1μs      | 243,902/s  | 1x (baseline) |
| Majavat (InputStream) | 5.8s       | 5.8μs      | 172,414/s  | 1.41x slower  |
| Selmer                | 128.6s     | 128.6μs    | 7,776/s    | 31.37x slower |
| Hiccup                | 13.7s      | 13.7μs     | 72,993/s   | 3.34x slower  |

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
