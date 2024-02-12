test_that("errors if credentials exist", {
  skip_on_cran()
  skip_on_runiverse()

  expect_error(
    set_matos_credentials(),
    "Some MATOS credentials already exist"
  )
})
