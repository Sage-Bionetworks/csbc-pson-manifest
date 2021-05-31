#' validator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_validator_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        box(
          title = "Instructions",
          width = "100%", collapsible = TRUE, collapsed = TRUE,
          p("In order to add new metadata to the portal, the values must
               first be validated. Select the type of manifest to be uploaded,
               then upload the file. A file preview will be displayed on the
               right. Click on ", strong("Validate."), "If all checks pass,
               feel free to upload; otherwise, edit accordingly then re-upload
               the manifest to validate again."),
          p(strong("Note:"), "the file must be .xlsx. If needed, templates
               are available below:"),
          tags$ul(
            tags$li(a(href = "www/templates/publications_manifest.xlsx",
                      "Publications", download = NA, target = "_blank")),
            tags$li(a(href = "www/templates/datasets_manifest.xlsx", "Datasets",
                      download = NA, target = "_blank")),
            tags$li(a(href = "www/templates/files_manifest.xlsx", "Files",
                      download = NA, target = "_blank")),
            tags$li(a(href = "www/templates/tools_manifest.xlsx", "Tools",
                      download = NA, target = "_blank")),
            tags$li(a(href = "www/templates/additional_standard_terms.xlsx",
                      "Additional Terms", download = NA, target = "_blank"))
          ),
          span(style = "font-size:smaller",
               em("Templates last updated Mar 2021."))
        ),

        span(style = "font-size:smaller",
             strong("Important!"), br(),
             "When uploading multiple manifests, upload the publications one first."
        ), br(), br(),

        selectInput(
          ns("manifest_type"),
          label = "Manifest type:",
          choices = c(
            "--choose one--" = "",
            "Publications" = "publication",
            "Datasets" = "dataset",
            "Files" = "file",
            "Tools" = "tool"
          ),
          selected = ""
        ),

        fileInput(
          ns("manifest_file"),
          label = "Upload manifest (.xlsx)",
          accept = c(
            ".xlsx",
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" #nolint
          )
        ),

        fileInput(
          ns("new_cv_terms"),
          label = "Additional Standard Terms (.xlsx)",
          accept= c(
            ".xlsx",
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" #nolint
          )
        ),

        div(
          align = "center",
          hidden(
            span(id = "missing_warning", style = "color:red",
                 em("File is missing a `standard_terms` sheet. See ",
                    strong("Instructions"), " for more details."), br()
            )
          ),
          hidden(
            span(id = "empty_warning", style = "color:red",
                 em("File is empty. Double-check the manifest and try again."),
                 br()
            )
          ),
          disabled(
            actionButton(
              ns("validate_btn"),
              label = "Validate",
              class = "btn-primary", style = "color: white"
            )
          )
        )
      ),

      mainPanel(
        tabBox(
          id = ns("validator_tabs"),
          width = "100%",
          tabPanel(
            "File Preview",
            value = "preview_tab",
            style = "height:78vh; overflow-y:scroll; overflow-x:scroll;",
            DT::DTOutput(ns("preview")),
            DT::DTOutput(ns("diag"))
          ),
          tabPanel(
            "Standard Terms",
            value = "terms_tab",
            style = "height:78vh; overflow-y:scroll; overflow-x:scroll;",
            p("Displayed below is the list of standard terms used to validate
                the manifest."),
            DT::DTOutput(ns("terms")),
          ),
          tabPanel(
            "Validation Results",
            value = "results_tab",
            style = "height:78vh",
            results_boxes_ui(ns("validation_results"))
          )
        )
      )
    )
  )
}

#' validator Server Function
#'
#' @noRd
mod_validator_server <- function(input, output, session, values){
  ns <- session$ns

  state <- reactiveValues()
  state$type_selected <- FALSE
  state$validate_ready <- FALSE

  output$terms <- DT::renderDT(
    isolate(values$cv_terms),
    options = list(pageLength = 50)
  )

  ## BUTTON ACTIVATION ########
  observe({
    toggleState("validate_btn", condition = state$validate_ready)
  })

  ## MANIFEST VALIDATOR #######
  v_waiter <- Waiter$new(
    id = "shiny-tab-validator",
    html = tagList(
      spin_loaders(15),
      h4("Validating...")
    ),
    color = "#424874"
  )

  manifest <- reactive({
    if (is.null(input$manifest_file$datapath)) {
      return(NULL)
    }
    result <- try(readxl::read_excel(input$manifest_file$datapath))
    if(is.data.frame(result)) {
      result <- result %>%
        dplyr::mutate_all(~ tidyr::replace_na(.x, "")) %>%
        plyr::rename(
          replace = c(
            fileURL = "fileUrl", datasetURL = "datasetUrl",
            toolName = "tool", homepageUrl = "externalLink",
            dpgapAccns = "dbgapAccns", dpgapUrls = "dbgapUrls"
          ),
          warn_missing = FALSE
        )
      return(result)
    } else {
      return(NULL)
    }
  })

  observeEvent(input$manifest_type, {
    if (is.null(input$manifest_type)) {
      state$type_selected <- FALSE
    } else if (input$manifest_type != "") {
      state$type_selected <- TRUE
    }
  })

  observeEvent(input$manifest_file, {
    if (!is.null(input$manifest_file$datapath)) {
      # input$manifest_file
      state$manifest_uploaded <- TRUE
      hide("missing_warning")
      hide("empty_warning")

      ### Show warning if uploaded manifest is empty
      if (nrow(manifest()) == 0) {
        show("empty_warning")
      } else {
        if (state$type_selected) {
          state$validate_ready <- TRUE
        }
        output$preview <- DT::renderDT(manifest())
      }
    }

    updateTabsetPanel(session, "validator_tabs", selected = "preview_tab")
  })

  observeEvent(input$new_cv_terms, {
    tryCatch({
      new_terms <- readxl::read_excel(input$new_cv_terms$datapath)
      if (nrow(new_terms) > 0) {
        new_terms <- new_terms %>%
          dplyr::select(
            one_of(c("Category", "standard_name", "key", "value"))
          ) %>% #nolint
          plyr::rename(
            replace = c(Category = "key", standard_name = "value"),
            warn_missing = FALSE
          ) %>%
          dplyr::mutate(
            key = stringr::str_replace_all(
              key,
              c(
                "outDataType" = "outputDataType",
                "Assay" = "assay",
                "Tumor Type" = "tumorType"
                )
            ) #nolint
          ) %>%
          tibble::add_column(columnType = "STRING")
        values$cv_terms <- unique(bind_rows(cv_terms, new_terms))
      }
    }, error = function(err) {
      showModal(
        modalDialog(
          title = "Uh oh, something went wrong!",
          span(
            "File of additional standard terms is not correctly formatted.
            There should be a column named `Category` or `key` and another
            column named `standard_name` or `value`.", br(), br(), "See",
            strong("Instructions"), "for a template file."),
          easyClose = TRUE
        )
      )
    })
  })

  observeEvent(input$validate_btn, {
    v_waiter$show()
    type <- input$manifest_type

    results <- list(
      missing_cols = check_col_names(
        manifest(),
        template[[type]],
        success_msg = "All required columns are present",
        fail_msg = "Missing columns in the manifest"
      ),
      invalid_cols = check_annotation_keys(
        manifest(),
        isolate(values$cv_terms),
        whitelist_keys = c(setdiff(template[[type]], std_cols), "Notes", "notes"),
        success_msg = "All column names are valid",
        fail_msg = "Some column names are invalid",
        annots_link = "https://www.synapse.org/#!Synapse:syn25322361/tables/"
      ),
      incomplete_cols = check_cols_complete(
        manifest(),
        required_cols = complete_cols[[type]],
        success_msg = "All necessary columns have annotations",
        fail_msg = "Some necessary columns are missing annotations"
      ),
      invalid_vals = check_listed_values(
        manifest(),
        isolate(values$cv_terms)
      )
    )
    if (type != "file") {
      results$dup_ids = check_id_dups(manifest(), id[[type]])
    }
    updateTabsetPanel(session, "validator_tabs", selected = "results_tab")
    callModule(results_boxes_server, "validation_results", results)
    next_step_modal(results, type)
    v_waiter$hide()
  })

  ## UPLOAD MANIFEST #######
  u_waiter <- Waiter$new(
    id = "next_step",
    html = div(style = "color:#465362",
               spin_loaders(15, color = "#465362"),
               h4("Uploading...")
    ),
    color = "white"
  )

  observeEvent(input$upload_btn, {
    u_waiter$show()
    type <- input$type

    apply(manifest(), 1, function(row) {

      name <- row[[ id[[type]] ]]

      if (type == "dataset") {
        annotations <- dataset_annots(row)
        syn_id <- save_folder_to_synapse(
          syn,
          synapseclient,
          name,
          parent_folder[["dataset"]],
          annotations
        )
      } else if (type == "file") {

        # Get parent ID (dataset folder ID) for data file
        res <- tables$datasets[grepl(paste0("^", row[["datasetId"]], "$"), tables$datasets[["datasetAlias"]], ignore.case = TRUE), ][["datasetId"]] #nolint
        dataset_folder <- unlist(res[!is.na(res)]) %>%
          stringr::str_replace_all(c("\\[" = "", "\\\"" = "", "\\]" = ""))

        annotations <- file_annots(row, dataset_folder, tables$grants, tables$datasets)
        syn_id <- save_file_to_synapse(
          syn,
          synapseclient,
          name,
          dataset_folder,
          annotations
        )
      } else {

        ### TODO: Skip making entities for publications and tools for now
        #        if (type == "publication") {
        #          name <- paste0("pmid_", name)
        #        }
        #        annotations <- switch(type,
        #          "publication" = publication_annots(row),
        #          "tool" = tool_annots(row, tables$grant)
        #        )
        #        syn_id <- save_file_to_synapse(
        #          synapseclient,
        #          name,
        #          parent_folder[[type]],
        #          annotations
        #        )
        syn_id <- ""
      }

      if (type != "file") {
        new_portal_row <- switch(type,
                                 "publication" = publication_row(
                                   syn_id, row,
                                   tables$grants,
                                   tables$datasets
                                 ),
                                 "dataset" = dataset_row(
                                   syn_id, row,
                                   tables$publications
                                 ),
                                 "tool" = tool_row(
                                   syn_id, row,
                                   tables$publications
                                 )
        )
        syn$store(synapseclient$Table(portal_table[[type]], new_portal_row))
      }
      #      output$diag <- DT::renderDT(new_portal_row)
      #      syn_store(synapseclient$Table(portal_table[[type]], new_portal_row))

      ### update stats on overview tab
      values$tables <- get_tables(syn)
      # TODO: make this tables value a reactive
      # display_overview_stats(output, tables)
      # display_quickview(output, tables)
    })

    u_waiter$update(
      html = div(style = "color:#465362",
                 span(style = "color:green", icon("check-circle", "fa-2x")),
                 strong("Upload complete!")
      )
    )
  })
}

