## Create tibble for entry into * - Merged tables.
publication_row <- function(syn_id, manifest, grants, datasets) {
  tibble(
    publicationId = syn_id,
    doi = manifest[["doi"]],
    journal = manifest[["journal"]],
    pubMedId = manifest[["pubMedId"]],
    pubMedUrl = manifest[["pubMedUrl"]],
    pubMedLink = create_url_markdown(paste0("PMID:", manifest[["pubMedId"]]), manifest[["pubMedUrl"]]), #nolint
    publicationTitle = manifest[["publicationTitle"]],
    publicationYear = manifest[["publicationYear"]],
    keywords = manifest[["keywords"]],
    authors = manifest[["authors"]],
    assay = create_listed_annots(manifest[["assay"]]),
    tumorType = create_listed_annots(manifest[["tumorType"]]),
    tissue = create_listed_annots(manifest[["tissue"]], delim =";"),
    themeId = split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "themeId"), #nolint
    theme = create_listed_annots(split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "theme")), #nolint
    consortiumId = split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "consortiumId"), #nolint
    consortium = create_listed_annots(manifest[["consortium"]]),
    grantId = create_listed_annots(manifest[["grantId"]]),
    grantName = create_listed_annots(split_and_search(manifest[["grantNumber"]], grants, "grantNumber", "grantName")), #nolint
    grantNumber = create_listed_annots(manifest[["grantNumber"]]),
    grantInstitution = create_listed_annots(split_and_search(
      manifest[["grantNumber"]], grants,
      "grantNumber", "grantInstitution"
    )),
    datasetId = ifelse(
      manifest[["bioProjectAccns"]] %in% datasets$datasetAlias,
      split_and_search(manifest[["bioProjectAccns"]], datasets, "datasetAlias", "datasetId"), # nolint
      ""
    ),
    dataset = manifest[["dataset"]]
  )
}

dataset_row <- function(syn_id, manifest, publications) {
  tibble(
    datasetId = syn_id,
    datasetName = manifest[["datasetName"]],
    datasetAlias = manifest[["datasetId"]],
    description = manifest[["description"]],
    overallDesign = manifest[["overallDesign"]],
    assay = create_listed_annots(manifest[["assay"]]),
    species = create_listed_annots(manifest[["species"]]),
    tumorType = create_listed_annots(manifest[["tumorType"]]),
    themeId = split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "themeId"), #nolint
    theme = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "theme")), #nolint
    consortiumId = split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "consortiumId"), #nolint
    consortium = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "consortium")), #nolint
    grantId = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "grantId")), #nolint
    grantName = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "grantName")), #nolint
    grantNumber = ifelse(
      manifest[["grantNumber"]] == "",
      create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "grantNumber")), #nolint
      create_listed_annots(manifest[["grantNumber"]])
    ),
    #publicationId = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "publicationId")), #nolint
    publicationId = NA,
    publicationTitle = create_listed_annots(manifest[["publicationTitle"]]),
    publication = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "pubMedLink", remove_chars = FALSE)), #nolint
    externalLink = ifelse(
      manifest[["externalLink"]] == "",
      create_url_markdown(
        paste0(
          if (startsWith(manifest[["datasetId"]], "GSE")) {
            "GEO:"
          } else if (startsWith(manifest[["datasetId"]], "SRP")) {
            "SRA:"
          } else {
            ""
          },
          manifest[["datasetId"]]
        ),
        manifest[["datasetUrl"]]
      ),
      manifest[["externalLink"]]
    )
  )
}

tool_row <- function(syn_id, manifest, publications) {
  tibble(
    toolId = syn_id,
    toolName = manifest[["tool"]],
    description = manifest[["description"]],
    homepageUrl = manifest[["externalLink"]],
    toolType = manifest[["toolType"]],
    softwareLanguage = create_listed_annots(manifest[["softwareLanguage"]]),
    inputDataType = create_listed_annots(manifest[["inputDataType"]]),
    outputDataType = create_listed_annots(manifest[["outputDataType"]]),
    themeId = split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "themeId"), #nolint
    theme = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "theme")), #nolint
    consortiumId = split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "consortiumId"), #nolint
    consortium = split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "consortium"), #nolint
    grantId = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "grantId")), #nolint
    grantName = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "grantName")), #nolint
    grantNumber = create_listed_annots(manifest[["grantNumber"]]),
    #publicationId = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "publicationId")), #nolint
    publicationId = NA,
    publicationTitle = create_listed_annots(manifest[["publicationTitle"]]),
    publication = create_listed_annots(split_and_search(manifest[["publicationTitle"]], publications, "publicationTitle", "pubMedLink", remove_chars = FALSE)), #nolint
  )
}
