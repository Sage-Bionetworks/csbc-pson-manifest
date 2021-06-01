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
