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
