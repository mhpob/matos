test_that("login gets cookie", {
  matos_login()

  url <- 'https://matos.asascience.com/report/submit'
  response <- httr::HEAD(url)

  expect_gt(nrow(response$cookies), 1)
})
