
<!-- README.md is generated from README.Rmd. Please edit that file -->

# listviewerlite

<!-- badges: start -->
<!-- badges: end -->

`listview()` creates a collapsible tree view of R lists using only HTML
and CSS, without the need for JavaScript.

## Installation

You can install the development version of listviewerlite from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("long39ng/listviewerlite")
```

## Example

``` r
library(listviewerlite)

x <- list(
  list(id = "a", val = 2),
  list(
    id = "b",
    val = 1,
    children = list(
      list(id = "b1", val = 2.5),
      list(
        id = "b2",
        val = 8,
        children = list(
          list(id = "b21", val = 4)
        )
      )
    )
  ),
  list(
    id = "c",
    val = 8,
    children = list(
      list(id = "c1"),
      list(id = "c2", val = 1)
    )
  )
)

listview(x)
```

<ul class="tree">
<li>
<details open="true">
<summary>
<span>
<code class="vec-header">&lt;list [1:3]&gt;</code>
</span>
</summary>
<ul>
<li>
<details>
<summary>
<span>
<code class="vec-header">&lt;named list [1:2]&gt;</code>
</span>
</summary>
<ul>
<li>
<span>
id
<code class="vec-header">&lt;character [1]&gt;</code>
</span>
<span class="vec-data">"a"</span>
</li>
<li>
<span>
val
<code class="vec-header">&lt;double [1]&gt;</code>
</span>
<span class="vec-data">2</span>
</li>
</ul>
</details>
</li>
<li>
<details>
<summary>
<span>
<code class="vec-header">&lt;named list [1:3]&gt;</code>
</span>
</summary>
<ul>
<li>
<span>
id
<code class="vec-header">&lt;character [1]&gt;</code>
</span>
<span class="vec-data">"b"</span>
</li>
<li>
<span>
val
<code class="vec-header">&lt;double [1]&gt;</code>
</span>
<span class="vec-data">1</span>
</li>
<li>
<details>
<summary>
<span>
children
<code class="vec-header">&lt;list [1:2]&gt;</code>
</span>
</summary>
<ul>
<li>
<details>
<summary>
<span>
<code class="vec-header">&lt;named list [1:2]&gt;</code>
</span>
</summary>
<ul>
<li>
<span>
id
<code class="vec-header">&lt;character [1]&gt;</code>
</span>
<span class="vec-data">"b1"</span>
</li>
<li>
<span>
val
<code class="vec-header">&lt;double [1]&gt;</code>
</span>
<span class="vec-data">2.5</span>
</li>
</ul>
</details>
</li>
<li>
<details>
<summary>
<span>
<code class="vec-header">&lt;named list [1:3]&gt;</code>
</span>
</summary>
<ul>
<li>
<span>
id
<code class="vec-header">&lt;character [1]&gt;</code>
</span>
<span class="vec-data">"b2"</span>
</li>
<li>
<span>
val
<code class="vec-header">&lt;double [1]&gt;</code>
</span>
<span class="vec-data">8</span>
</li>
<li>
<details>
<summary>
<span>
children
<code class="vec-header">&lt;list [1]&gt;</code>
</span>
</summary>
<ul>
<li>
<details>
<summary>
<span>
<code class="vec-header">&lt;named list [1:2]&gt;</code>
</span>
</summary>
<ul>
<li>
<span>
id
<code class="vec-header">&lt;character [1]&gt;</code>
</span>
<span class="vec-data">"b21"</span>
</li>
<li>
<span>
val
<code class="vec-header">&lt;double [1]&gt;</code>
</span>
<span class="vec-data">4</span>
</li>
</ul>
</details>
</li>
</ul>
</details>
</li>
</ul>
</details>
</li>
</ul>
</details>
</li>
</ul>
</details>
</li>
<li>
<details>
<summary>
<span>
<code class="vec-header">&lt;named list [1:3]&gt;</code>
</span>
</summary>
<ul>
<li>
<span>
id
<code class="vec-header">&lt;character [1]&gt;</code>
</span>
<span class="vec-data">"c"</span>
</li>
<li>
<span>
val
<code class="vec-header">&lt;double [1]&gt;</code>
</span>
<span class="vec-data">8</span>
</li>
<li>
<details>
<summary>
<span>
children
<code class="vec-header">&lt;list [1:2]&gt;</code>
</span>
</summary>
<ul>
<li>
<details>
<summary>
<span>
<code class="vec-header">&lt;named list [1]&gt;</code>
</span>
</summary>
<ul>
<li>
<span>
id
<code class="vec-header">&lt;character [1]&gt;</code>
</span>
<span class="vec-data">"c1"</span>
</li>
</ul>
</details>
</li>
<li>
<details>
<summary>
<span>
<code class="vec-header">&lt;named list [1:2]&gt;</code>
</span>
</summary>
<ul>
<li>
<span>
id
<code class="vec-header">&lt;character [1]&gt;</code>
</span>
<span class="vec-data">"c2"</span>
</li>
<li>
<span>
val
<code class="vec-header">&lt;double [1]&gt;</code>
</span>
<span class="vec-data">1</span>
</li>
</ul>
</details>
</li>
</ul>
</details>
</li>
</ul>
</details>
</li>
</ul>
</details>
</li>
</ul>
<!-- ![](man/figures/preview.gif){width=480px} -->

## Acknowledgements

This package is inspired by
[{listviewer}](https://github.com/timelyportfolio/listviewer), which
wraps the JavaScript libraries
[jsoneditor](https://github.com/josdejong/jsoneditor) and
[react-json-view](https://github.com/mac-s-g/react-json-view).

The implementation of the `listview()` function is inspired by
`utils::str()` and `lobstr::tree()`.

The custom CSS properties for the collapsible tree view are copied and
modified from the [blog post by Kate Rose
Morley](https://iamkate.com/code/tree-views/).
