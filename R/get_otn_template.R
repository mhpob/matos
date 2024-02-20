#' Download Ocean-Tracking-Network-style metadata templates
#'
#' @param template_type Character string. One of: "receiver" (default), the
#'      deployment data submittal template; "tag", the tagging data submittal
#'      template; or "glider", the wave and Slocum glider metadata template.
#' @param out_dir Optional character string noting where you would like the file
#'      to be downloaded. Defaults to the working directory.
#' @param quiet suppress status messages from `download.file`
#'
#' @return Ocean Tracking Network metadata template in XLSX format.
#'
#' @examplesIf all(skip_example_on_cran(), skip_example_on_runiverse())
#' # Tag metadata template downloaded to working directory
#' get_otn_template()
#'
#' # Glider metadata template downloaded to temporary directory
#' get_otn_template("glider", out_dir = tempdir())
#' @export
get_otn_template <- function(template_type = c("receiver", "tag", "glider"),
                             out_dir = NULL, quiet = FALSE) {
  # Check that arguments are correct
  template_type <- match.arg(template_type)

  # Check that user is logged in
  login_check()

  # Convert template type to filename (as of 2020-11-02)
  template_file <- switch(template_type,
    receiver = "otn_metadata_deployment.xlsx",
    tag = "otn_metadata_tagging.xlsx",
    glider = "glider-deployment-metadata-v2.xlsx"
  )

  out_filepath <- ifelse(
    is.null(out_dir),
    file.path(getwd(), template_file),
    file.path(out_dir, template_file)
  )

  # Download the file
  download.file(
    paste("https://matos.asascience.com/static", template_file, sep = "/"),
    destfile = out_filepath,
    mode = "wb", quiet = quiet
  )

  out_filepath
}
