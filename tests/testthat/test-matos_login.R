test_that("login gets cookie", {
  # skip test on Runiverse
  skip_on_Runiverse()

  matos_login()

  url <- 'https://matos.asascience.com/report/submit'
  response <- httr::HEAD(url)

  expect_gt(nrow(response$cookies), 1)
})
