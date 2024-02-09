## ACT project
test_that("ACT projects are summarized", {
  skip_on_cran()

  expect_no_error(
    matos_tag_summary(
      matos_project = 87
    )
  )

  expect_true(any(grepl("tag_push_summary", list.files(getwd()))))
})
