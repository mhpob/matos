test_that("logoff works", {
  matos_login()

  matos_logoff()

  url <- 'https://matos.asascience.com/report/submit'
  response <- httr::HEAD(url)

  expect_equal(nrow(response$cookies), 1)
})
