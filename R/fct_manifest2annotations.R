## Set annotations for a given entity, depending on manifest type.
publication_annots <- function(manifest) {
  list(
    doi = manifest[["doi"]],
    title = manifest[["publicationTitle"]],
    journal = manifest[["journal"]],
    publicationYear = manifest[["publicationYear"]],
    keywords = manifest[["keywords"]],
    pubMedUrl = manifest[["pubMedUrl"]],
    pubMedLink = manifest[["pubMedUrl"]],
    assay = manifest[["assay"]],
    tissue = manifest[["tissue"]],
    tumorType = manifest[["tumorType"]]
  )
}


dataset_annots <- function(manifest) {
  list(
    fullName = manifest[["datasetName"]],
    displayName = manifest[["datasetId"]]
  )
}


tool_annots <- function(manifest, grants) {
  # some tools may be annotated with multiple grants.
  split_grants <- lapply(
    stringr::str_split(manifest[["grantNumber"]], ","), trimws
  )[[1]]
  list(
    displayName = manifest[["tool"]],
    grantId = paste(grants[grants$grantNumber == split_grants, ]$grantId, collapse = ", "), #nolint
    toolType = manifest[["toolType"]]
  )
}


file_annots <- function(manifest, parent, grants, datasets) {
  list(
    fileName = manifest[["fileName"]],
    name = manifest[["fileName"]],
    datasets = split_and_search(manifest[["datasetName"]], datasets, "datasetName", "datasetAlias"), #nolint
    parentId = parent,
    assay = manifest[["assay"]],
    platform = manifest[["platform"]],
    dataFormat = manifest[["dataFormat"]],
    species = manifest[["species"]],
    gender = manifest[["sex"]],
    tumorType = manifest[["tumorType"]],
    tissue = manifest[["tissue"]],
    grantName = split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "grantName"), #nolint
    grantType = split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "grantType"), #nolint
    consortium = split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "consortium") #nolint
  )
}
