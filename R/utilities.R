#' Log in to your MATOS account
#'
#' This function prompts you for the username (email) and password associated with
#' your MATOS account. This is necessary so that you may interface with any
#' project-specific files. If you don't have a MATOS account
#' \href{https://matos.asascience.com/account/signup}{you can sign up for one here}.
#' Log in is completed using the \href{https://rstudio.github.io/rstudio-extensions/rstudioapi.html}{RStudio API};
#' this probably won't work if you're not using RStudio, so it will be changed in the future.
#' A pop up will appear asking for your username and password. If everything works
#' out, your credentials will be kept in the sessions' cookies. Your username/password
#' will not be saved -- this was done intentionally so that you don't accidentally
#' save credentials in a public script.
#'
#' @examples
#' \dontrun{
#' # Type:
#' matos_login()
#' # ...then follow the on-screen prompts
#' }

matos_login <- function(){
  credentials <- list(
    UserName = getPass::getPass('Username:', noblank = T),
    Password = getPass::getPass('Password:', noblank = T)
  )


  login_response <- httr::POST(
    'https://matos.asascience.com/account/login',
    body = credentials,
    encode = 'form'
  )

  if('rstudioapi' %in% installed.packages()){
    if(grepl('login', login_response)){
      rstudioapi::showDialog('Login unsuccessful :(',
                             'Your username/password combination was not recognized.
                             Please re-run the funtion and try again.')
      stop('Login unsuccessful.')

    } else{
      rstudioapi::showDialog('Login successful!',
                             'You are now logged into your MATOS profile.')
    }
  } else{
    if(grepl('login', login_response)){
      tcltk::tk_messageBox('ok',
                           'Your username/password combination was not recognized.
                           Please re-run the funtion and try again.',
                           caption = 'Login unuccessful :(')
      stop('Login unsuccessful.')

    } else{
      tcltk::tk_messageBox('ok',
                           'You are now logged into your MATOS profile.',
                           caption = 'Login unuccessful !')
    }
  }
}



##' Create new MATOS project
##'
##' Somewhat useless function that just opens up the MATOS project creation page
# Doesn't work.
# matos_create <- function(){
#   # matos_login()
#   browseURL('http://matos.asascience.com/project/add')
# }


#' Internal functions used by \code{matos}
#'
#' Non-exported utility functions used by other functions in \code{matos}.
#'
#' @section Details:
#' \code{get_file_list} scrapes the HTML associated with the project or data
#' extraction files page provided with a given project.
#'
#' \code{get_project_number} finds the internal MATOS number associated with each
#' project by scraping the HTML of the main MATOS projects page.
#'
#' \code{html_table_to_df} converts the HTML table provided by \code{get_file_list}
#' into a R-usable data frame.
#'
#' \code{login_check} pings protected URLs and calls \code{matos_login} when referred
#' to the login page.
#'
#' \code{scrape_file_urls} is used internally by \code{html_table_to_df} to extract
#' the URLs associates with each "Download" link.
#'
#' @param project_number Number of the project
#' @param data_type one of "dataextractionfiles" or "projectfiles".
#' @param project Character string of the full MATOS project name. This will be the
#' big name in bold at the top of your project page, not the "Project Title" below it.
#' Will be coerced to all lower case, so capitalization doesn't matter.
#' @param url The (protected) URL that the overlapping function is trying to call.
#' @param html_file_list Listed files in HTML form. Always the result of
#' \code{get_file_list}
#'
#'
#' @name utilities

get_file_list <- function(project_number, data_type){

  url <- paste('https://matos.asascience.com/project',
               data_type,
               project_number, sep = '/')

  login_check(url)

  httr::GET(url)
}


#' @rdname utilities
#'
get_project_number <- function(project){
  projects <- matos_projects()
  projects[projects$name == tolower(project),]$number
}


#' @rdname utilities
#'
html_table_to_df <- function(html_file_list){

  df <- httr::content(html_file_list, 'parsed') %>%
    rvest::html_nodes('.tableContent') %>%
    rvest::html_table() %>%
    data.frame()
  names(df) <- tolower(gsub('\\.', '_', names(df)))


  df <- df[, names(df) != 'var_4']
  df$upload_date <- as.Date(df$upload_date, format = '%m/%d/%Y')
  df$project <- gsub('.*/', '', html_file_list$url)

  urls <- scrape_file_urls(html_file_list)

  df <- cbind(df, url = urls)

  # Parse file name for data extraction files
  if(any(unique(df$file_type) == 'Data Extraction File')){
    df <- data.frame(detection_type = gsub('.*\\d_|_det.*', '', df$file_name),
                     detection_year = as.numeric(gsub('.*_|.zip', '', df$file_name)),
                     df)
    df <- df[, c('project', 'file_type', 'detection_type', 'detection_year',
                 'upload_date', 'file_name', 'url')]
  }else{
    df <- df[, c('project', 'file_type', 'upload_date', 'file_name', 'url')]
  }


  df
}


#' @rdname utilities
#'
login_check <- function(url = 'https://matos.asascience.com/report/submit'){

  check_response <- httr::HEAD(url)

  if(nrow(check_response$cookies) == 1){
    message('Please log in.')

    matos_login()
  }

}

#' @rdname utilities
#'
scrape_file_urls <- function(html_file_list){
  urls <- httr::content(html_file_list, 'parsed') %>%
    rvest::html_node('body') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('href')

  urls <- grep('projectfile', urls, value = T)

  paste0('https://matos.asascience.com', urls)
}
