#' List personal MATOS projects
#'
#' This function lists the functions for which the logged-on user has permissions
#'
#'
#' @export
#' @examples
#' \dontrun{
#' # After logging in, just type the following:
#' get_my_projects()
#' }
get_my_projects <- function(){
  url <- 'https://matos.asascience.com/report/submit'

  login_check(url)

  site <- httr::GET(url)

  names <- httr::content(site) %>%
    rvest::html_node(xpath = '//*[@id="selProject"]') %>%
    rvest::html_nodes('option') %>%
    rvest::html_text()

  all_projects <- matos_projects()

  all_projects[all_projects$name %in% tolower(names),]
}



#' List MATOS project files
#'
#' These functions list the file names, types, upload date, and URLs of MATOS
#' project files -- basically everything you see in the *Data Extraction Files*
#' or *Project Files* sections of your project page. Because it is from your
#' project page, you **need to** first log in using \code{\link{matos_login}}.
#'
#' @section Details:
#' \code{detection_files} and \code{project_files} are wrappers around a
#' web-scraping routine: 1) find the project number if not provided, 2) download
#' the HTML table, 3) parse the URL for each file, and 4) combine the table and
#' URLs into a data frame. These functions are most useful when investigating what
#' files you have available, and then downloading them with \code{\link{get_file}}.
#'
#' \code{detection_files} lists files associated with the ACT_MATOS OTN node. These
#' are files listed on the *Data Extraction Files* page.
#'
#' \code{project_files} lists tag and receiver metadata files that have been
#' uploaded by the user. These are the files listed on the *Project Files* page.
#'
#' @param project Either the project number (the number in your project page URL)
#'     or the full name of the project (the big name in bold on your project page,
#'     *not* the "Project Title").
#' @param data_type one of "extraction" (default), or "project", which will list
#'     the data extraction or project files, respectively. Partial matching
#'     is allowed, and will repair to the correct argument if spaces or the words
#'     "data"/"file(s)" are included.
#' @param since Only list files uploaded after this date. Optional, but must be
#'      in YYYY-MM-DD format.
#'
#' @return A data frame with columns of "File.Name", "File.Type", "Upload.Date", and "url".
#'
#' @export
#' @examples
#' \dontrun{
#' # Select using project number, this defaults to grabbing the data extraction files
#' list_files(87)
#'
#' # Or, grab the project files
#' list_files(project = 87, data_type = 'project')
#'
#' # Select using project name
#' list_files('umces boem offshore wind energy')
#' }

list_files <- function(project = NULL, data_type = c('extraction', 'project'),
                       since = NULL){

  # Check and coerce data_type
  data_type <- gsub(' |file[s]?|data', '', data_type)
  data_type <- match.arg(data_type)
  data_type <- ifelse(data_type == 'extraction', 'dataextractionfiles',
                      'downloadfiles')

  # Convert project name to number
  if(is.character(project)){
    project <- get_project_number(project)
  }

  # Scrape table and list files
  # This calls login_check() under the hood
  files_html <- get_file_list(project, data_type = data_type)

  files <- html_table_to_df(files_html)

  if(!is.null(since)){
    files <- files[files$upload_date >= since, ]
  }

  files

}



#' List all MATOS projects
#'
#' This function scrapes the table found at \url{https://matos.asascience.com/project}.
#' This table provides not only the full name of the project, but also the MATOS
#' project number and project page URL. You do not need to log in via \code{matos_login}
#' or have any permissions to view/download this table.
#'
#' @export
#' @examples
#' \dontrun{
#' # Just type in the following...
#' matos_projects()
#' }
matos_projects <- function(){
  project_list <- httr::GET(
    'https://matos.asascience.com/project'
  )

  projects_info <- httr::content(project_list) %>%
    rvest::html_node('.project_list') %>%
    rvest::html_nodes('a')

  urls <- rvest::html_attr(projects_info, 'href')

  projects <- data.frame(
    name = tolower(rvest::html_text(projects_info, trim = T)),
    number = gsub('.*detail/', '', urls),
    url = paste0('https://matos.asascience.com',
                 urls)
  )

  projects
}
