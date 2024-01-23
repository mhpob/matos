test_that("logoff works", {
  # skip test on Runiverse
  skip_on_Runiverse()

  matos_login()

  matos_logoff()

  url <- "https://matos.asascience.com/report/submit"
  response <- httr::HEAD(url)

  expect_false(
    any(grepl("AUTH", response$cookies$name))
  )
})
