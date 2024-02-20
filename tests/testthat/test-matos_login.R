test_that("gets cookie", {
  skip_on_cran()
  skip_on_runiverse()

  matos_login()

  url <- "https://matos.asascience.com/report/submit"
  response <- httr::HEAD(url)

  expect_true(
    any(grepl("AUTH", response$cookies$name))
  )
})




test_that("errors with incorrect creds", {
  skip_if_offline()
  skip_on_runiverse()

  expect_error(
    matos_login(credentials = list(
      UserName = "BarneyRubble",
      Password = "betty4EVA!"
    )),
    "Login unsuccessful"
  ) |>
    expect_warning(
      "You have provided your credentials"
    )
})
