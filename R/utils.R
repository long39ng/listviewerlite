str_truncate <- function(x, n) {
  ifelse(nchar(x) > n,
    gsub("\\s*$", "\u2026", substr(x, 1, n - 1), perl = TRUE),
    x
  )
}

vec_truncate <- function(x, n) {
  x <- as.character(x)
  x <- ifelse(is.na(x), paste0("<em>", x, "</em>"), x)
  if (length(x) > n) {
    x <- c(utils::head(x, n), "\u2026")
  }
  paste0(x, collapse = ", ")
}

is_named <- function(x) {
  !is.null(names(x))
}
