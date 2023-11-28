.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    cli::cli_text(
      "By continuing, you are agreeing to the ACT Network MATOS User Agreement and Data Policy, Version 1.2:\n\n",
      "{.url https://matos.asascience.com/static/MATOS.User.Agreement.V1.1.pdf}"
    )
  )

  if(!curl::has_internet()){
    packageStartupMessage(
      cli::cli_alert_warning(
        'No internet connection detected. Package functionality will be limited.'
      )
    )
  }
}

.onLoad <- function(libname, pkgname) {
  list_my_projects <<- memoise::memoise(list_my_projects)
  list_projects <<- memoise::memoise(list_projects)
  get_file_list <<- memoise::memoise(get_file_list)
}
