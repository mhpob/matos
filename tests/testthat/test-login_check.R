test_that("runs matos_login", {
  skip_on_cran()

  matos_logoff()

  expect_message(
    login_check(),
    "Please log in."
  ) |>
    expect_message("Login successful!")
})


test_that("ne message if already logged in", {
  skip_on_cran()

  matos_login()

  expect_no_message(
    login_check()
  )
})
