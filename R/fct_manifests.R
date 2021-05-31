#' manifests
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
create_table_row <- function(table_columns, manifest_row) {
  string_cols <- c("STRING", "LARGETEXT", "ENTITYID")
  col_names <- purrr::map(table_columns, "name")
  purrr::map(table_columns, function(col_schema) {
    val <- manifest_row[[col_schema["name"]]]
    val <- if_else(is.null(val), "", val)
    if (col_schema[["columnType"]] %in% string_cols) {
      val
    } else if (stringr::str_detect(col_schema[["columnType"]], "LIST")) {
      .delim_str_to_json(val)
    } else {
      val
    }
  }) %>%
    purrr::set_names(col_names) %>%
    tibble::as_tibble()
}
