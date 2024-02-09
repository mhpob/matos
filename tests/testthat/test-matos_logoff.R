test_that("logoff works", {
  skip_on_cran()

  matos_login()

  matos_logoff()

  url <- "https://matos.asascience.com/report/submit"
  response <- httr::HEAD(url)

  expect_false(
    any(grepl("AUTH", response$cookies$name))
  )
})
