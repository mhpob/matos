#' Download project files from the MATOS website
#'
#' \code{get_project_file} downloads files from the MATOS website. This is best-used in
#' conjunction with \code{\link{list_project_files}}.
#'
#' @param file A character vector listing the name of the file, or a numeric listing
#'     the index as found from \code{\link{list_project_files}}.
#' @param project A character vector listing the full name of the project, or a
#'     numeric listing the project number.
#' @param url The URL of the file to be downloaded.
#' @param out_dir Character. What directory/folder do you want the file saved into?
#'      Default is the current working directory.
#' @param overwrite Logical. Do you want a file with the same name overwritten?
#'      Passed to httr::write_disk.
#' @param quiet Logical. Do you want to silence matos' updates? Default is FALSE.
#'
#' @export
#' @examplesIf all(skip_example_on_cran(), skip_example_on_runiverse())
#' # If you know the index of the file, you can provide some numbers
#' get_project_file(file = 1, project = 87)
#'
#' # If you know the direct URL to your file, you don't need the file or project names:
#' get_project_file(url = "https://matos.asascience.com/projectfile/download/327")
get_project_file <- function(file = NULL, project = NULL,
                             url = NULL, out_dir = getwd(),
                             overwrite = FALSE, quiet = FALSE) {
  # Check that only one file or URL has been provided
  if (length(file) > 1 || length(url) > 1) {
    cli::cli_abort(c(
      "Only one file can be extracted at a time.",
      "Try looping with `for` or `lapply`."
    ))
  }

  # If calling the URL directly:
  if (!is.null(url)) {
    login_check(url)

    download_process(
      url = url, out_dir = out_dir, overwrite = overwrite, quiet = quiet
    )
  } else {
    # If providing a project name or number and a file/index instead of the URL:

    # Check that both file and project are provided
    if (is.null(file) | is.null(project)) {
      stop("Need a file name/index and its project name/number.")
    }



    if (is.character(project)) {
      project <- get_project_number(project)
    }

    # As well as getting the file list, this will call login_check() to check credentials
    file_html <- get_file_list(project, "downloadfiles")

    file_table <- html_table_to_df(file_html)

    if (is.numeric(file)) {
      # Check that index exists in the table.
      if (file == 0 | file > nrow(file_table)) {
        stop(paste0(
          "There is no index matching what you have provided. ",
          "Try a file name or index from 1 to ",
          nrow(file_table), "."
        ))
      }

      file_url <- file_table[file, ]$url

      download_process(
        url = file_url, out_dir = out_dir, overwrite = overwrite, quiet = quiet
      )
    } else {
      # Check that file exists in the table.
      if (!any(grepl(file, file_table$file_name, ignore.case = TRUE))) {
        stop(paste0(
          "There is no file matching what you have provided, please ",
          "double-check your file name."
        ))
      }

      file_url <- file_table[grep(file, file_table$file_name,
        ignore.case = TRUE
      ), ]$url

      download_process(
        url = file_url, out_dir = out_dir, overwrite = overwrite, quiet = quiet
      )
    }
  }
}
