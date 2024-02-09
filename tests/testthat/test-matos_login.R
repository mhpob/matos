test_that("login gets cookie", {
  skip_on_cran()

  matos_login()

  url <- "https://matos.asascience.com/report/submit"
  response <- httr::HEAD(url)

  expect_true(
    any(grepl("AUTH", response$cookies$name))
  )
})
