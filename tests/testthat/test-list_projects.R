test_that("no error in listing projects", {
  # skip test on Runiverse
  skip_on_Runiverse()

  expect_no_error({
    list_projects()
  })
})


test_that("returns correct classes", {
  # skip test on Runiverse
  skip_on_Runiverse()

  projects <- list_projects()

  expect_s3_class(projects, "data.frame")
  expect_type(projects$name, "character")
  expect_type(projects$number, "double")
  expect_type(projects$url, "character")
})
