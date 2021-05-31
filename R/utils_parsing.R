#' Convert comma-separated string values into a JSON array string
#'
#' @param str A string with values separated by commas
#'
#' @return The string containing a JSON array of the values
#' @export
#'
#' @examples
#' .delim_str_to_json("foo, bar, baz")
.delim_str_to_json <- function(str, delimiter = ",") {
  json_str <- str_split(str, delimiter)[[1]] %>%
    str_trim() %>%
    jsonlite::toJSON()
  if (json_str == "[null]" || json_str == '[""]') {
    ""
  } else {
    json_str
  }
}


## Prettify a string that will cater to Synapse naming rules, e.g.
## can only contain letters, numbers, spaces, underscores, hypens,
## periods, plus signs, apostrophes, and parentheses.
syn_prettify <- function(name) {
  stringr::str_replace_all(name, c(":" = "-", ";" = "-", "/" = "_"))
}


## Create a "list" of annotations; some of the table schemas require
## a column to be a StringList, e.g. '["a", "b"]' instead of 'a, b'.
create_listed_annots <- function(annots, delim = ",") {
  split_annots <- lapply(stringr::str_split(annots, delim), trimws)[[1]]
  res <- paste0(
    "[\"",
    paste0(split_annots, collapse = "\", \""),
    "\"]"
  )
  res
}


## Create markdown for URL
create_url_markdown <- function(link_text, url, param = "") {
  paste0("[", link_text, "](", url, param, ")")
}


## Split annots and search for match in table
split_and_search <- function(annots, table, search_col, output_col,
                             remove_chars = TRUE) {
  res <- lapply(stringr::str_split(annots, ", ")[[1]], function(annot) {
    stringr::str_split(
      table[grepl(paste0("^", annot, "$"), table[[search_col]], ignore.case = TRUE), ][[output_col]], #nolint
      ", "
    )
  })

  # get rid of NAs and unnecessary characters
  res <- unlist(res[!is.na(res)])
  if (isTRUE(remove_chars)) {
    res <- stringr::str_replace_all(res, c("\\[" = "", "\\\"" = "", "\\]" = ""))
  }
  paste0(unique(res), collapse = ", ")
}
