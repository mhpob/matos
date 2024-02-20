#' Log in to your MATOS account
#'
#' This function prompts you for the username (email) and password associated with
#' your MATOS account. This is necessary so that you may interface with any
#' project-specific files. If you don't have a MATOS account
#' \href{https://matos.asascience.com/account/signup}{you can sign up for one here}.
#'
#' A pop up will appear asking for your username and password. If everything works
#' out, your credentials will be kept in the sessions' cookies. Your username/password
#' will not be saved -- this was done intentionally so that you don't accidentally
#' save credentials in a public script.
#'
#' @param credentials list with names "UserName" and "Password". This argument
#'    only exists for testing purposes and should not be used! It will store your
#'    credentials in your R history, which is definitely not good.
#'
#' @export
#' @examples
#' \dontrun{
#' # Type:
#' matos_login()
#' # ...then follow the on-screen prompts
#' }
matos_login <- function(credentials = NULL) {
  cli::cli_alert_warning("Please log in.")

  if (is.null(credentials)) {
    # This uses a secret to allow vignettes to build and tests to run
    username <- Sys.getenv("MATOS_USER")
    password <- Sys.getenv("MATOS_PASS")

    # If no secret or credentials are present, we enter interactive mode
    if (username == "") {
      username <- getPass::getPass("Username:", noblank = T)
    }
    if (password == "") {
      password <- getPass::getPass("Password:", noblank = T)
    }

    credentials <- list(
      UserName = username,
      Password = password
    )
  } else {
    warning(
      paste("You have provided your credentials as an argument to this function.",
        "Because your credentials are now stored in your R history, this is risky and only an option for testing purposes.",
        "Consider wiping your history and providing credentials interactively or in your .Reviron.",
        sep = "\n"
      )
    )
  }

  login_response <- httr::POST(
    "https://matos.asascience.com/account/login",
    body = credentials,
    encode = "form"
  )

  if (grepl("login", login_response)) {
    cli::cli_abort("Login unsuccessful.",
      "i" = "Please re-run the funtion and try again."
    )
  } else {
    cli::cli_alert_success("Login successful!")
  }
}

#' Log out of your MATOS account
#'
#' This function takes no arguments -- just tells MATOS that you want to log out. Useful if you're changing users or on a public computer and would like to protect your projects.
#'
#' @export
#' @examples
#' \dontrun{
#' matos_logoff()
#' }
matos_logoff <- function() {
  logoff_response <- httr::GET(
    "https://matos.asascience.com/account/logoff"
  )

  cli::cli_alert_success("Logged out.")
}




#' Internal functions used by \code{matos}
#'
#' Non-exported utility functions used by other functions in \code{matos}.
#'
#' @section Details:
#' \code{get_file_list} checks to see if it should re-evaluate itself, then wraps
#' `get_file_list_mem` which is the actual workhorse.
#'
#' \code{get_file_list_mem} memoised function which scrapes the HTML associated
#' with the project or data extraction files page provided with a given project.
#'
#' \code{get_project_number} finds the internal MATOS number associated with each
#' project by scraping the HTML of the main MATOS projects page.
#'
#' \code{get_project_name} finds the MATOS project name associated with the given
#' project number by scraping the HTML of the main MATOS projects page.
#'
#' \code{html_table_to_df} converts the HTML table provided by \code{get_file_list}
#' into a R-usable data frame.
#'
#' \code{login_check} pings protected URLs and calls \code{matos_login} when referred
#' to the login page.
#'
#' \code{project_check}
#'
#' \code{scrape_file_urls} is used internally by \code{html_table_to_df} to extract
#' the URLs associates with each "Download" link.
#'
#' \code{download_process} is used internally by \code{get_project_file} and
#' \code{get_extract_file}
#'
#' @name utilities

#' @param project_number Number of the project
#' @param data_type one of "dataextractionfiles" for OTN detection extracts
#'      or "downloadfiles" for the uploaded project files.
#' @param force Do you want to reset the cache and re-ping the database?
#'      Defaults to false.
get_file_list <- function(project_number, data_type, force = FALSE) {
  if (isTRUE(force)) {
    memoise::forget(get_file_list_mem)
  }

  get_file_list_mem(
    project_number,
    data_type
  )
}

#' #inheritParams get_file_list
#' @rdname utilities
get_file_list_mem <- function(project_number, data_type) {
  url <- paste("https://matos.asascience.com/project",
    data_type,
    project_number,
    sep = "/"
  )

  login_check(url)

  file_list <- httr::GET(url)

  content_check <- httr::content(file_list)
  content_check <- rvest::html_element(content_check,
    xpath = '//*[@id="content"]/table'
  )

  if (inherits(content_check, "xml_missing")) {
    stop(
      "No file lists returned.\nAre you using the correct project ID?"
    )
  }

  file_list
}


#' @param project_name Character string of the full MATOS project name. This will be the
#'      big name in bold at the top of your project page, not the "Project Title"
#'      below it. Will be coerced to all lower case, so capitalization doesn't matter.
#' @param matos_projects Data frame. Used to pass the MATOS project list from
#'      \code{project_check}.
#' @rdname utilities
get_project_number <- function(project_name, matos_projects = NULL) {
  if (is.null(matos_projects)) {
    matos_projects <- list_projects(quiet = TRUE)
  }

  matos_projects_clean <- tolower(matos_projects$name)
  project_name_clean <- tolower(trimws(project_name))

  number <- matos_projects[matos_projects_clean == project_name_clean, ]$number

  if (length(number) == 0) {
    collection_code <- tolower(matos_projects$collectioncode)

    number <- matos_projects[collection_code == project_name_clean &
      !is.na(collection_code), ]$number

    if (length(number) == 0) {
      possible_matches <- matos_projects[
        c(
          agrep(project_name_clean, matos_projects_clean, 0.25),
          agrep(project_name_clean, collection_code)
        ), "name"
      ]

      if (length(possible_matches) != 0) {
        cli::cli_abort(c(
          "No projects matched with \"{project_name}\".",
          "Perhaps you meant one of the following:\n\n{possible_matches}"
        ))
      } else {
        cli::cli_abort(
          "No projects matched with \"{project_name}\"."
        )
      }
    }
  }

  number
}


#' #inheritParams get_file_list
#' #inheritParams get_project_number
#' @rdname utilities
#'
get_project_name <- function(project_number, matos_projects = NULL) {
  if (is.null(matos_projects)) {
    matos_projects <- list_projects(quiet = TRUE)
  }

  name <- matos_projects[matos_projects$number == project_number, ]$name

  if (length(name) == 0) {
    cli::cli_abort(
      "No projects matched with \"{project_number}\"."
    )
  }

  name
}


#' @param html_file_list Listed files in HTML form. Always the result of
#' \code{get_file_list}
#' @rdname utilities
#'
html_table_to_df <- function(html_file_list) {
  df <- httr::content(html_file_list, "parsed")
  df <- rvest::html_element(df, xpath = '//*[@id="content"]/table')
  df <- rvest::html_table(df)
  df <- data.frame(df)
  names(df) <- tolower(gsub("\\.", "_", names(df)))

  df <- df[, names(df) != "var_4"]
  df$upload_date <- as.Date(df$upload_date, format = "%m/%d/%Y")

  # Make a blank data frame if there are no files to be listed.
  if (nrow(df) == 0) {
    df <- cbind(
      df,
      data.frame(
        project = character(),
        url = character()
      )
    )

    if (grepl("dataextractionfiles", html_file_list$url)) {
      df <- cbind(
        df,
        data.frame(
          detection_type = character(),
          detection_year = numeric()
        )
      )
      df <- df[, c(
        "project", "file_type", "detection_type", "detection_year",
        "upload_date", "file_name", "url"
      )]
    } else {
      df <- df[, c("project", "file_type", "upload_date", "file_name", "url")]
    }
  } else {
    # otherwise continue to parse
    df$project <- gsub(".*/", "", html_file_list$url)

    urls <- scrape_file_urls(html_file_list)

    df <- cbind(df, url = urls)

    # Parse file name for data extraction files
    if (grepl("dataextractionfiles", html_file_list$url)) {
      df <- data.frame(
        detection_type = gsub(
          ".*_(.*)+_detections.*", "\\1",
          df$file_name
        ),
        detection_year = as.numeric(
          gsub(".*_|.zip", "", df$file_name)
        ),
        df
      )
      df <- df[, c(
        "project", "file_type", "detection_type", "detection_year",
        "upload_date", "file_name", "url"
      )]
    } else {
      df <- df[, c("project", "file_type", "upload_date", "file_name", "url")]
    }
  }

  df
}


#' @param url The (protected) URL that the overlapping function is trying to call.
#' @rdname utilities
#'
login_check <- function(url = "https://matos.asascience.com/report/submit") {
  check_response <- httr::HEAD(url)

  if (!any(grepl("AUTH", check_response$cookies$name))) {
    matos_login()
  }
}

#' @param project MATOS project ID. Can be the name or number of the project.
#' @param return_projects Logical. Do you want \code{project_check} to return the list
#'      of projects? Used to not ping the website too much in one function call.
#' @rdname utilities
#'
project_check <- function(project, return_projects = FALSE) {
  matos_projects <- list_projects(quiet = TRUE)

  if (is.character(project)) {
    matos_projects_clean <- tolower(matos_projects$name)
    search_project_clean <- tolower(trimws(project))

    if (!(search_project_clean %in% matos_projects_clean)) {
      cli::cli_abort(paste(
        project,
        "was not found among the MATOS project names."
      ))
    }
  }

  if (is.numeric(project)) {
    if (!(project %in% matos_projects$number)) {
      cli::cli_abort(paste(
        project,
        "was not found among the MATOS project numbers."
      ))
    }
  }

  if (return_projects == TRUE) {
    matos_projects
  }
}

#' #inheritParams html_file_to_df
#' @rdname utilities
#'
scrape_file_urls <- function(html_file_list) {
  urls <- httr::content(html_file_list, "parsed")
  urls <- rvest::html_node(urls, "body")
  urls <- rvest::html_nodes(urls, "a")
  urls <- rvest::html_attr(urls, "href")

  urls <- grep("projectfile", urls, value = T)

  paste0("https://matos.asascience.com", urls)
}

#' #inheritParams login_check
#' @param out_dir Character. To what directory would you like your files downloaded?
#'      Defaults to the current working directory.
#' @param overwrite Logical. Do you want to overwrite existing files that have
#'      the same name (\code{TRUE}) or protect yourself against doing this
#'      (\code{FALSE}, the default)?
#' @param to_vue Logical. Should the data be converted to match that of VUE's
#'      CSV export? Defaults to FALSE.
#' @param quiet Logical. Do you want to silence matos' updates? Default is FALSE.
#' @rdname utilities
#'
download_process <- function(url, out_dir, overwrite,
                             to_vue = FALSE, quiet = FALSE) {
  if (isFALSE(quiet)) cli::cli_h1("Downloading files")

  GET_header <- httr::GET(url)

  response <- httr::GET(
    url,
    httr::write_disk(
      path = file.path(
        out_dir,
        gsub(
          '.*filename=|\\"', "",
          httr::headers(GET_header)$"content-disposition"
        )
      ),
      overwrite = overwrite
    )
  )

  file_loc <- file.path(response$content)

  if (isFALSE(quiet)) {
    cli::cli_alert_success("\nFile(s) saved to:")
    cat("  ", paste(file_loc, collapse = "\n   "), "\n")
  }

  if (grepl("zip", file_loc)) {
    if (isFALSE(quiet)) cli::cli_h1("Unzipping files")

    file_loc <- unzip(file_loc, exdir = out_dir, setTimes = FALSE)

    if (isFALSE(quiet)) {
      cli::cli_alert_success("\nFile(s) unzipped to:")
      cat("  ", paste(file_loc, collapse = "\n   "), "\n")
    }
  }

  if (isTRUE(to_vue) && any(grepl("matched_external", file_loc))) {
    if (isFALSE(quiet)) {
      cli::cli_warn(
        c(
          "!" = "Detections that have been \"matched to external partners\" are provided in a summary format.",
          "!" = "Conversion to VUE CSV format will not take place."
        ),
        wrap = TRUE
      )
    }
  } else {
    if (isTRUE(to_vue)) {
      if (isFALSE(quiet)) cli::cli_h1("Converting to VUE CSV format")

      file_csv <- grep(".csv", file_loc, value = T)
      matos <- read.csv(
        file_csv
      )


      matos$transmitter.name <- ""
      matos$transmitter.serial <- ""

      type <- gsub(
        "(.*?)_(.*)_detections.*$", "\\2",
        basename(file_csv)
      )

      if (type %in% c("sentinel_tag", "unqualified")) {
        matos$sensorraw <- ""
      }
      if (type %in% c("qualified", "sentinel_tag", "unqualified")) {
        matos$sensorunit <- ""
      }


      columns_to_include <- switch(type,
        matched =
          c(
            "datecollected", "receiver", "tagname", "transmitter.name",
            "transmitter.serial", "sensorraw", "sensorunit", "station",
            "latitude", "longitude"
          ),
        qualified =
          c(
            "datecollected", "collectornumber", "fieldnumber",
            "transmitter.name", "transmitter.serial", "sensorraw", "sensorunit",
            "station", "latitude", "longitude"
          ),
        sentinel_tag =
          c(
            "datecollected", "collectornumber", "fieldnumber",
            "transmitter.name", "transmitter.serial", "sensorraw", "sensorunit",
            "station", "latitude", "longitude"
          ),
        unqualified =
          c(
            "datecollected", "collectornumber", "fieldnumber",
            "transmitter.name", "transmitter.serial", "sensorraw", "sensorunit",
            "station", "latitude", "longitude"
          )
      )

      matos <- matos[, columns_to_include]

      names(matos) <- c(
        "Date and Time (UTC)", "Receiver", "Transmitter",
        "Transmitter Name", "Transmitter Serial", "Sensor Value",
        "Sensor Unit", "Station Name", "Latitude", "Longitude"
      )

      file_csv <- file.path(
        dirname(file_csv),
        paste0("vue_", basename(file_csv))
      )

      write.csv(matos, file_csv, row.names = F)
      if (isFALSE(quiet)) cli::cli_alert_success("CSV converted to VUE format.")

      file_loc <- c(file_loc, file_csv)
    }
  }


  file_loc
}


#' Place where functions live for the matos_*_summary family of functions
#'
#' @param type type of data: qualified or unqualified detections; or deployment metadata
#' @param matos_project matos project number
#' @param project_files qualified/unqualified detection files
#' @param temp_dir location of temporary directory
#'
#' @keywords internal

act_file_download <- function(type, temp_dir = NULL, matos_project = NULL,
                              project_files = NULL) {
  cli::cli_alert_info(paste("Downloading", type, "detections..."))


  if (type == "deployment") {
    # list project files and select deployment metadata
    files <- list_project_files(matos_project)

    files <- files[grepl("Deployment", files$file_type), ]
  } else {
    # select the appropriate detection type
    files <- project_files[project_files$detection_type == type, ]
  }

  # ping the server and download the file(s).
  files <- lapply(
    files$url,
    function(.) {
      get_extract_file(
        url = .,
        out_dir = temp_dir
      )
    }
  )


  files <- unlist(files)

  if (type != "deployment") {
    files <- grep("\\.csv$", files, value = T)
  }

  cli::cli_alert_success("   Done.")

  files
}


#' Miscellaneous functions for package checking, building, and CI
#'
#' `skip_example_on_ci` and `skip_example_on_runiverse` check the environment for
#'    variables called "CI" and "MY_UNIVERSE", respectively, and return `TRUE`
#'    if it does not exist. Used to run examples if the package is being built
#'    locally and there's a chance that `vdat.exe` exists. If the package is being built on
#'    a continuous integration platform like GitHub Actions, the "CI" variable
#'    will be `TRUE` and `skip_example_on_ci` will return `FALSE`. If it is being
#'    built locally, "CI" will be "" and `skip_example_on_ci` will return
#'    `TRUE`. Similarly, if the package is being built on R-Universe, the
#'    "MY_UNIVERSE" variable will have your universe's name.
#'
#' @name CI_utilities
#' @export

skip_example_on_ci <- function() {
  Sys.getenv("CI") == ""
}

#' @rdname CI_utilities
#' @export

skip_example_on_runiverse <- function() {
  Sys.getenv("MY_UNIVERSE") == ""
}

#' @rdname CI_utilities
#' @export

skip_example_on_cran <- function() {
  # Logic borrowed from testthat::skip_on_cran
  !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
}
