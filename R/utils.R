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


## Save new entity to Synapse; Folder for dataset manifest, File for others
save_file_to_synapse <- function(syn, synapseclient, name,
                                 parent, annotations) {
  name <- syn_prettify(name)

  # create dummy file to upload to Synapse
  write(name, file = name)
  new_file <- synapseclient$File(
    path = name,
    name = name,
    parent = parent,
    annotations = annotations
  )
  new_file <- syn$store(new_file)

  # remove dummy file
  file.remove(name)

  new_file$id
}


save_folder_to_synapse <- function(syn, synapseclient, name,
                                   parent, annotations) {
  name <- syn_prettify(name)
  new_folder <- synapseclient$Folder(
    name,
    parent = parent,
    annotations = annotations
  )
  new_folder <- syn$store(new_folder)
  new_folder$id
}


#' Get Synapse tables used for the portal.
get_tables <- function(syn) {
  grants <- get_portal_table(
    syn,
    portal_table[["grant"]],
    c("grantId", "grantName", "grantNumber", "grantInstitution",
      "themeId", "theme", "consortiumId", "consortium")
  )
  publications <- get_portal_table(
    syn,
    portal_table[["publication"]],
    c("publicationId", "publicationTitle", "grantId", "grantNumber",
      "grantName", "themeId", "theme", "consortiumId", "consortium"
    )
  )
  datasets <- get_portal_table(
    syn,
    portal_table[["dataset"]],
    c("datasetId", "datasetName", "datasetAlias")
  )
  tools <- get_portal_table(
    syn,
    portal_table[["tool"]],
    c("toolId", "toolName")
  )
  files <- get_portal_table(
    syn,
    portal_table[["file"]],
    c("fileName", "datasets", "parentId")
  )
  list(
    "grants" = grants,
    "publications" = publications,
    "datasets" = datasets,
    "files" = files,
    "tools" = tools
  )
}


#' Get selected coloumns from a Synapse table.
get_portal_table <- function(syn, table_id, cols) {
  query <- sprintf("SELECT %s FROM %s", paste0(cols, collapse=","), table_id)
  return(syn$tableQuery(query)$asDataFrame())
}


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


# modal with next step (based on dccvalidator's next_step_modal)
next_step_modal <- function(results, type) {
  is_failure <- purrr::map_lgl(results, function(x) {
    inherits(x, "check_fail")
  })
  instructions = tagList(
    p("You may now upload the manifest by clicking the button below."),
    actionButton("upload_btn", class="btn-lg btn-block",
                 icon = icon("cloud-upload-alt"), "Upload to Synapse")
  )
  if (length(results) & !any(is_failure)) {
    Sys.sleep(1)
    showModal(
      modalDialog(
        id = "next_step",
        title = "Validation Checks Passed!",
        instructions,
        easyClose = TRUE
      )
    )
  }
}
