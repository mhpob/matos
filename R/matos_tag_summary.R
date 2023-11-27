#' Create summary reports of receiver project data from the OTN data push
#'
#' @param matos_project MATOS project number or name that you wish to have summarized
#' @param matched Default is NULL: OTN matched detections will be downloaded from MATOS and unzipped. If you do not wish to download your files, this argument also accepts a character vector of file paths of your matched detections. These can be CSVs or zipped folders.
#' @param ... Arguments passed to \code{otndo::make_tag_push_summary}
#'
#' @export
#' @examples
#' \dontrun{
#' # You can just use your ACT project number
#' matos_tag_summary(87)
#'
#' # Or provide an optional date to summarize "What's New".
#' matos_tag_summary(87, since = "2018-11-01")
#' }
matos_tag_summary <- function(
    matos_project = NULL,
    matched = NULL,
    ...) {
  if (all(is.null(matos_project), is.null(matched))) {
    cli::cli_abort("Must provide an ACT/MATOS project or at least one set of OTN-matched data.")
  }


  # Project ----
  ##  Find project name
  if (is.numeric(matos_project)) {
    project_number <- matos_project
    project_name <- get_project_name(matos_project)
  }
  if (is.character(matos_project)) {
    project_name <- matos_project
    project_number <- get_project_number(matos_project)
  }


  if (is.null(matched)) {
    cli::cli_alert_info("Finding extraction files...")
    matched <- list_extract_files(project_number, "all")
    cli::cli_alert_success("   Files found.\n")

    td <- file.path(tempdir(), "matos_files")

    # remove previous files. Needed if things errored out.
    if (file.exists(td)) {
      unlink(td, recursive = T)
    }

    dir.create(td)


    matched <- act_file_download(
      type = "matched",
      project_files = matched,
      temp_dir = td
    )
  }

  otndo::make_tag_push_summary(matched = matched, ...)
}
