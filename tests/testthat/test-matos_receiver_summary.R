## ACT project
test_that("ACT projects are summarized", {
  skip_on_cran()

  expect_no_error(
    matos_receiver_summary(
      matos_project = 161
    )
  )

  expect_true(any(grepl("receiver_push_summary", list.files(getwd()))))
})

## ACT project with receiver deployment data in unlabeled sheet
test_that("ACT project with unlabeled deployment sheet is summarized", {
  skip_on_cran()

  expect_no_error(
    matos_receiver_summary(
      matos_project = 164
    )
  )

  expect_true(any(grepl("receiver_push_summary", list.files(getwd()))))
})
