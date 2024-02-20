#' Download all data extraction files that were updated after a certain date
#'
#' This is a loop around \code{\link{get_extract_file}}.
#'
#' @inheritParams list_extract_files
#' @param out_dir Character. What directory/folder do you want the file saved into?
#'      Default is the current working directory. Passed to \code{httr::write_disk}
#'      via \code{\link{get_extract_file}}.
#' @param overwrite Logical. Do you want a file with the same name overwritten?
#'      Passed to \code{httr::write_disk} via \code{\link{get_extract_file}}.
#' @param to_vue Logical. Should the data be converted to match that of VUE's
#'      CSV export?
#' @param progress Logical. Do you want a progress bar? Default is TRUE.
#' @param quiet Logical. Do you want to silence matos' updates? Default is TRUE.
#'
#' @returns Saves the requested files to your computer and provides a list of
#'      file paths of the downloaded files.
#'
#' @examplesIf all(skip_example_on_cran(), skip_example_on_runiverse())
#' # Download files from the MDWEA project updated in the November 2023 data push
#' #   (you'll need to use a project for which you have permissions).
#' get_extract_updates(project = "MDWEA", since = "2023-11-01")
#'
#' # Match the VUE CSV export style
#' get_extract_updates(project = 160, to_vue = TRUE)
#'
#' @export
get_extract_updates <- function(project,
                                since = NULL,
                                detection_type = c(
                                  "all", "matched", "external",
                                  "qualified", "sentinel", "unqualified"
                                ),
                                out_dir = getwd(),
                                overwrite = FALSE,
                                to_vue = FALSE,
                                progress = TRUE,
                                quiet = TRUE) {

  files <- list_extract_files(project, detection_type, since)

  if(is.null(since)) {
    cli::cli_bullets(c(
      "x" = "No \"since\" date has been provided.",
      "!" = "All extract files will be downloaded!"
    ))
  }

  if (nrow(files) == 0) {
    cli::cli_alert_success("No files uploaded since the provided date.")
  } else {
    file_location <- vector("list", length(files$url))
    if (isTRUE(progress)) {
      cli::cli_progress_bar("Downloading files",
                            type = "tasks",
                            total = length(files$url))
    }
    for (i in seq_along(files$url)) {

      file_location[[i]] <- get_extract_file(
        url = files$url[i],
        out_dir = out_dir, overwrite = overwrite,
        to_vue = to_vue,
        quiet = quiet
      )
      if (isTRUE(progress)) cli::cli_progress_update()
    }

    if (isTRUE(progress)) cli::cli_progress_done()

    file_location
  }
}
