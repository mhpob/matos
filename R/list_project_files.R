#' List MATOS project files
#'
#' This function lists the file names, types, upload date, and URLs of MATOS
#' project files -- basically everything you see in the *Project Files* section
#' of your project page. Because it is from your project page, you will be
#' prompted to log in.
#'
#' @section Details:
#' \code{list_project_files} is a wrapper around a web-scraping routine:
#' 1) find the project number if not provided, 2) download
#' the HTML table, 3) parse the URL for each file, and 4) combine the table and
#' URLs into a data frame. This function is most useful when investigating what
#' files you have available, and then downloading them with
#' \code{\link{get_project_file}}.
#'
#' \code{list_project_files} lists tag and receiver metadata files that have been
#' uploaded by the user. These are the files listed on the *Project Files* section
#' of your project page.
#'
#' @param project Either the project number (the number in your project page URL)
#'     or the full name of the project (the big name in bold on your project page,
#'     *not* the "Project Title").
#' @param file_type one of, or a vector of, "all" (default), "detections",
#'    "receiver_metadata", or "tag_metadata". Partial matching is
#'    allowed.
#' @param since Only list files uploaded after this date. Optional, but must be
#'      in YYYY-MM-DD format.
#'
#' @return A data frame with columns of "project", "file_type", "upload_date", and "file_name".
#'
#' @export
#' @examplesIf all(skip_example_on_cran(), skip_example_on_runiverse())
#' # List files using project number:
#' list_project_files(87)
#'
#' # Or using the project name
#' list_project_files("umces boem offshore wind energy")
#'
#' # List only the receiver deployment metadata files
#' List_project_files(87, "receiver_metadata")
#'
#' # List both the receiver and transmitter deployment metadata files
#' List_project_files(87, c("receiver_metadata", "tag_metadata"))
#'
#' # Cheat and use shorter names
#' List_project_files(87, c("receiver", "tag"))
list_project_files <- function(project = NULL,
                               file_type = c(
                                 "all", "detections",
                                 "receiver_metadata",
                                 "tag_metadata"
                               ),
                               since = NULL) {
  # Check and coerce input args
  file_type <- match.arg(file_type, c(
    "all", "detections",
    "receiver_metadata",
    "tag_metadata"
  ), several.ok = TRUE)

  file_type_fix <- function(provided_type) {
    switch(provided_type,
           detections = "Tag Detections - .vfl file",
           receiver_metadata = "Deployed Receivers \u2013 Deployment Metadata",
           tag_metadata = "Tagged Fish \u2013 Tag Metadata"
    )
  }

  file_type <- sapply(
    file_type,
    file_type_fix,
    USE.NAMES = F
  )

  # Convert project name to number
  if (is.character(project)) {
    project <- get_project_number(project)
  }

  # Scrape table and list files
  # This calls login_check() under the hood
  files_html <- get_file_list(project, data_type = "downloadfiles")

  files <- html_table_to_df(files_html)

  if (all(file_type != "all")) {
    files <- files[files$file_type %in% file_type, ]
  }

  if (!is.null(since)) {
    # Check that date is in YYYY-MM-DD format
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", since)) {
      warning(
        paste0(
          "The \"since\" date was not provided in YYYY-MM-DD format.",
          "\nAll files have been returned."
        )
      )
    } else {
      files <- files[files$upload_date >= since, ]
    }
  }

  files
}
