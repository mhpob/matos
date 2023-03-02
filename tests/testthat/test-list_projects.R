test_that('no error in listing projects', {
  expect_no_error({
    list_projects()
  })
})


test_that("returns correct classes", {

  projects <- list_projects()

  expect_s3_class(projects, 'data.frame')
  expect_type(projects$name, 'character')
  expect_type(projects$number, 'double')
  expect_type(projects$url, 'character')
})
