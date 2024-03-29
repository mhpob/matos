% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{matos_login}
\alias{matos_login}
\title{Log in to your MATOS account}
\usage{
matos_login(credentials = NULL)
}
\arguments{
\item{credentials}{list with names "UserName" and "Password". This argument
only exists for testing purposes and should not be used! It will store your
credentials in your R history, which is definitely not good.}
}
\description{
This function prompts you for the username (email) and password associated with
your MATOS account. This is necessary so that you may interface with any
project-specific files. If you don't have a MATOS account
\href{https://matos.asascience.com/account/signup}{you can sign up for one here}.
}
\details{
A pop up will appear asking for your username and password. If everything works
out, your credentials will be kept in the sessions' cookies. Your username/password
will not be saved -- this was done intentionally so that you don't accidentally
save credentials in a public script.
}
\examples{
\dontrun{
# Type:
matos_login()
# ...then follow the on-screen prompts
}
}
