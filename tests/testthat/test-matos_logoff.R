test_that("logoff works", {
  skip_on_cran()
  skip_on_runiverse()

  matos_login()

  expect_message(
    matos_logoff(),
    "Logged out\\."
  )

  url <- "https://matos.asascience.com/report/submit"
  response <- httr::HEAD(url)

  expect_false(
    any(grepl("AUTH", response$cookies$name))
  )
})
