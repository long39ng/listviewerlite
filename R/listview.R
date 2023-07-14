#' View object as collapsible nested list in HTML
#'
#' @param x An atomic vector or a list
#'
#' @inherit htmltools::tags return
#'
#' @export
#'
#' @examples
#' x <- list(
#'   list(id = "a", val = 2),
#'   list(
#'     id = "b",
#'     val = 1,
#'     children = list(
#'       list(id = "b1", val = 2.5),
#'       list(
#'         id = "b2",
#'         val = 8,
#'         children = list(
#'           list(id = "b21", val = 4)
#'         )
#'       )
#'     )
#'   ),
#'   list(
#'     id = "c",
#'     val = 8,
#'     children = list(
#'       list(id = "c1"),
#'       list(id = "c2", val = 1)
#'     )
#'   )
#' )
#'
#' listview(x)
listview <- function(x) {
  css <- htmltools::htmlDependency(
    name = "tree-views",
    version = "0.1.0",
    src = "dep",
    stylesheet = "tree-views.css",
    package = "listviewerlite"
  )

  ret <- tags$ul(class = "tree", tag(x), css)

  structure(ret, class = c("html_tree", class(ret)))
}

#' @export
print.html_tree <- function(x, ...) {
  htmltools::html_print(x)

  invisible(x)
}

tag <- function(x, name = NULL) {
  stopifnot(is.atomic(x) || typeof(x) == "list")

  if (!is.vector(x) && typeof(x) == "list") x <- as.list(x)

  if (is.null(x)) {
    tags$li(name, tags$code("NULL"))
  } else if (is.atomic(x)) {
    if (is.character(x) && length(x) > 0) {
      x <- gsub("\\n", "\u21B5", x, perl = TRUE)
      x <- str_truncate(x, if (length(x) == 1) 64 else 16)
      x <- paste0("\"", x, "\"")
    }
    tags$li(tag_header(x, name), vec_truncate(x, 8))
  } else {
    if (is_named(x)) {
      list_items <- mapply(tag, x, names(x), SIMPLIFY = FALSE, USE.NAMES = FALSE)
    } else {
      list_items <- lapply(x, tag)
    }
    tags$li(tags$details(tags$summary(tag_header(x, name)), tags$ul(list_items)))
  }
}

tag_header <- function(x, name) {
  type <- typeof(x)

  if (is_named(x)) type <- paste("named", type)

  tags$span(
    name,
    tags$code(paste0("<", type, if (length(x) > 1) " [1:" else " [", length(x), "]>"))
  )
}
