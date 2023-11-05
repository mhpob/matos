.onLoad <- function(libname, pkgname) {
  list_my_projects <<- memoise::memoise(list_my_projects)
  list_projects <<- memoise::memoise(list_projects)
  get_file_list <<- memoise::memoise(get_file_list)
}
