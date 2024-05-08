#' List personal MATOS projects
#'
#' This function lists the functions for which the logged-on user has permissions.
#'
#' @param read_access Do you want to only list projects for which you have
#'      file-read permission? Defaults to TRUE, though there is significant
#'      speed up if switched to FALSE.
#' @param force Do you want to reset the cache and re-ping the database?
#'      Defaults to false.
#' @param warn_multimatch Warn you if there have been multiple project matches?
#'      Defaults to TRUE.
#'
#' @export
#' @examplesIf all(skip_example_on_cran(), skip_example_on_runiverse())
#' # After logging in, just type the following:
#' list_my_projects()
list_my_projects <- function(read_access = TRUE,
                             force = FALSE,
                             warn_multimatch = TRUE) {
  if (isTRUE(force)) {
    memoise::forget(list_my_projects_mem)
  }

  list_my_projects_mem(
    read_access = read_access,
    warn_multimatch = warn_multimatch
  )
}

#' Memoised list_my_projects function for internal use
#'
#' @inheritParams list_my_projects
#'
#' @keywords internal
list_my_projects_mem <- function(read_access,
                                 warn_multimatch) {
  url <- "https://matos.asascience.com/report/submit"

  login_check(url)

  site <- httr::GET(url)

  names <- httr::content(site)
  names <- rvest::html_node(names, xpath = '//*[@id="selProject"]')
  names <- rvest::html_nodes(names, "option")
  names <- rvest::html_text(names)
  names <- names[names != ""]

  all_projects <- list_projects(
    what = "all", quiet = TRUE,
    warn_multimatch = warn_multimatch
  )

  if (read_access == T) {
    project_numbers <- unique(unlist(sapply(names, get_project_number)))

    # MATOS website issues code 302 and refers to project splash page if there is
    #   no read access. Capture which projects do this.
    files <- lapply(project_numbers, function(x) {
      httr::HEAD(
        url = paste("https://matos.asascience.com/project",
          "dataextractionfiles",
          x,
          sep = "/"
        ),

        # Don't follow referred URL to save time
        config = httr::config(followlocation = F)
      )
    })

    # Select projects that weren't referred
    files <- sapply(files, function(x) x$status_code != 302)

    project_numbers <- project_numbers[files]

    all_projects[all_projects$number %in% project_numbers, ]
  } else {
    all_projects[all_projects$name %in% names, ]
  }
}
