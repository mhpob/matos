#' Create summary reports of receiver project data from the OTN data push
#'
#' @param matos_project MATOS project number or name that you wish to have summarized
#' @param qualified,unqualified Default is NULL: OTN qualified or unqualified detections will be downloaded from MATOS and unzipped. If you do not wish to download your files (or you're not a member of ACT), this argument also accepts a character vector of file paths of your qualified/unqualified detections. These can be CSVs or zipped folders.
#' @param deployment File path of user-supplied master OTN receiver deployment metadata.
#' @param ... Arguments passed to \code{otndo::make_receiver_push_summary}
#'
#' @section No files provided:
#'
#'  If you only provide your ACT project number or title and leave all of the
#'  arguments as their defaults, this function will ask you to log in then proceed
#'  to download all of the necessary files. If you provide already-downloaded files
#'  you can speed up this process substantially.
#'
#' @section Output:
#'
#'  This function creates an HTML report that can be viewed in your web browser.
#'
#' @export
#' @examples
#' \dontrun{
#' # Using only the ACT/MATOS project number:
#' matos_receiver_summary(87)
#'
#' # Providing a local file:
#' matos_receiver_summary(87, deployment = "my_master_deployment_metadata.xlsx")
#'
#' # Get a summary fo what has changed since a particular date:
#' matos_receiver_summary(87, since = '2022-05-01')
#' }

matos_receiver_summary <- function(
    matos_project = NULL,
    qualified = NULL,
    unqualified = NULL,
    deployment = NULL,
    ...
){
  if(is.null(matos_project) &
     any(is.null(qualified), is.null(unqualified), is.null(deployment))){
    cli::cli_abort('Must provide an ACT/MATOS project or at least one each of qualified detections, unqualified detections, and deployment.')
  }


  # Create a temporary directory to store intermediate files
  td <- file.path(tempdir(), 'matos_files')

  # remove previous files. Needed if things errored out.
  if(file.exists(td)){
    unlink(td, recursive = T)
  }

  dir.create(td)

  # Project ----
  ##  Find project name
  if(is.numeric(matos_project)){
    project_number <- matos_project
    project_name <- get_project_name(matos_project)
    project_code <- paste0('PROJ', project_number)
  }
  if(is.character(matos_project)){
    project_name <- matos_project
    project_number <- get_project_number(matos_project)
    project_code <- paste0('PROJ', project_number)
  }


  if(any(is.null(qualified), is.null(unqualified))){
    cli::cli_alert_info('Finding extraction files...')
    project_files <- list_extract_files(project_number, 'all')
    cli::cli_alert_success('   Files found.')
  }



  # Qualified detections ----
  ##  Download qualified detections if not provided
  if(is.null(qualified)){
    qualified <- act_file_download(type = 'qualified',
                                   project_files = project_files,
                                   temp_dir = td)
  }

  # Unqualified detections ----
  ##  Download unqualified detections if not provided
  if(is.null(unqualified)){
    unqualified <- act_file_download(type = 'unqualified',
                                     project_files = project_files,
                                     temp_dir = td)
  }

  # Deployment log ----
  ##  Download deployment metadata if not provided
  if(is.null(deployment)){
    deployment <- act_file_download(type = 'deployment',
                                    matos_project = matos_project,
                                    temp_dir = td)
  }


  otndo::make_receiver_push_summary(qualified = qualified,
                                    unqualified = unqualified,
                                    deployment = deployment,
                                    ...)


}
