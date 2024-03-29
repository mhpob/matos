#' List OTN data extraction files
#'
#' This function list the file names, types, upload date, and URLs of OTN data
#' extraction files -- basically everything you see in the *Data Extraction Files*
#' section of your project page. Because it is from your project page, you will be
#' prompted to log in.
#'
#' @section Details:
#' \code{list_extract_files} is a wrapper around a web-scraping routine:
#' 1) find the project number if not provided, 2) download
#' the HTML table, 3) parse the URL for each file, and 4) combine the table and
#' URLs into a data frame. This function is most useful when investigating what
#' files you have available, and then downloading them with
#' \code{\link{get_extract_file}}.
#'
#' \code{list_extract_files} lists files associated with the ACT_MATOS OTN node. These
#' are files listed on the *Data Extraction Files* page.
#'
#' @param project Either the project code, the project number (the number in
#'      your project page URL), or the full name of the project (the big name in
#'      bold on your project page, *not* the "Project Title").
#' @param detection_type one of, or a vector of, "all" (default), "matched",
#'      "external", "qualified", "sentinel_tag", or "unqualified". Partial
#'      matching is allowed, and will repair to the correct argument if spaces
#'      or the words detection(s)" are included.
#'      More information on data types can be found on \href{https://members.oceantrack.org/data/otn-detection-extract-documentation-matched-to-animals}{OTN's website}.
#' @param since Only list files uploaded after this date. Optional, but must be
#'      in YYYY-MM-DD format.
#'
#' @return A data frame with columns of "project", "file_type", "detection_type", 'detection_year', 'upload_date', 'file_name', and "url".
#'
#' @export
#' @examplesIf all(skip_example_on_cran(), skip_example_on_runiverse())
#' # List all extraction files using project number
#' list_extract_files(87)
#'
#' # Or, just grab the matched receiver detections
#' list_extract_files(project = 87, detection_type = "qualified")
#'
#' # OR list files using the project name
#' list_extract_files("umces boem offshore wind energy")
list_extract_files <- function(project,
                               detection_type = c(
                                 "all", "matched", "external",
                                 "qualified", "sentinel",
                                 "unqualified"
                               ),
                               since = NULL) {
  # Check and coerce input args
  detection_type <- gsub(" |detection[s]", "", detection_type)
  detection_type <- match.arg(detection_type, several.ok = TRUE)

  if ("external" %in% detection_type) {
    detection_type[detection_type == "external"] <- "matched_external_partners"
  }
  if ("sentinel" %in% detection_type) {
    detection_type[detection_type == "sentinel"] <- "sentinel_tag"
  }

  # Make sure project exists
  # matos_projects <- project_check(project, return_projects = T)

  # Convert project name to number
  if (is.character(project)) {
    project <- get_project_number(project)
  }

  # Scrape table and list files
  # This calls login_check() under the hood
  files_html <- get_file_list(project, data_type = "dataextractionfiles")

  files <- html_table_to_df(files_html)

  if (all(detection_type != "all")) {
    files <- files[files$detection_type %in% detection_type, ]
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
