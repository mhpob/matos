% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_my_projects.R
\name{list_my_projects}
\alias{list_my_projects}
\title{List personal MATOS projects}
\usage{
list_my_projects(read_access = TRUE, force = FALSE, warn_multimatch = TRUE)
}
\arguments{
\item{read_access}{Do you want to only list projects for which you have
file-read permission? Defaults to TRUE, though there is significant
speed up if switched to FALSE.}

\item{force}{Do you want to reset the cache and re-ping the database?
Defaults to false.}

\item{warn_multimatch}{Warn you if there have been multiple project matches?
Defaults to TRUE.}
}
\description{
This function lists the functions for which the logged-on user has permissions.
}
\examples{
\dontshow{if (all(skip_example_on_cran(), skip_example_on_runiverse())) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# After logging in, just type the following:
list_my_projects()
\dontshow{\}) # examplesIf}
}
