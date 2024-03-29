## ACT project
test_that("ACT projects are summarized", {
  skip_on_cran()
  skip_on_runiverse()


  expect_message(
    matos_tag_summary(matos_project = 87),
    "Finding extraction files"
  ) |>
    expect_message("Files found") |>
    expect_message("Downloading matched detections") |>
    expect_message("Downloading files") |>
    expect_message("File\\(s\\) saved to") |>
    expect_message("Unzipping files") |>
    expect_message("File\\(s\\) unzipped to") |>
    expect_message("Asking OTN GeoServer for project information") |>
    expect_message("Writing report") |>
    expect_message("Done")

  expect_true(
    any(
      grepl("tag_push_summary", list.files(getwd()))
    )
  )
})




test_that("errors with no way to find files", {
  expect_error(
    matos_tag_summary(),
    "Must provide an ACT.*OTN-matched data"
  )
})
