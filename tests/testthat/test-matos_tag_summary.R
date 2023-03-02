## ACT project
test_that('ACT projects are summarized', {

  expect_no_error(
    matos_tag_summary(
      matos_project = 161
    )
  )

  expect_true(any(grepl('tag_push_summary', list.files(getwd()))))

})


unlink(td, recursive = T)
