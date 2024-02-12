## ACT project
test_that("ACT projects are summarized", {
  skip_on_cran()
  skip_on_runiverse()

  # matos::matos_receiver_summary messages
  expect_message(
    matos_receiver_summary(
      matos_project = 161
    ),
    "Finding extraction files"
  ) |>
    expect_message("Files found") |>
    expect_message("Downloading qualified detections") |>
    expect_message("Downloading files") |>
    expect_message("File\\(s\\) saved to") |>
    expect_message("Unzipping files") |>
    expect_message("File\\(s\\) unzipped to") |>
    expect_message("Done") |>
    expect_message("Downloading unqualified detections") |>
    expect_message("Downloading files") |>
    expect_message("File\\(s\\) saved to") |>
    expect_message("Unzipping files") |>
    expect_message("File\\(s\\) unzipped to") |>
    expect_message("Done") |>
    expect_message("Downloading deployment detections") |>
    expect_message("Downloading files") |>
    expect_message("File\\(s\\) saved to") |>
    expect_message("Unzipping files") |>
    expect_message("File\\(s\\) unzipped to") |>
    expect_message("Done") |>
    expect_message("Asking OTN GeoServer for project information") |>
    expect_message("Writing report") |>
    # otndo messages
    expect_output("processing file") |>
    expect_output("output file") |>
    expect_output("Output created") |>
    # matos again
    expect_message("Done")


  expect_true(
    any(
      grepl("receiver_push_summary", list.files(getwd()))
    )
  )
})

## ACT project with receiver deployment data in unlabeled sheet
test_that("ACT project with unlabeled deployment sheet is summarized", {
  skip_on_cran()
  skip_on_runiverse()

  suppressMessages(
    matos_receiver_summary(
      matos_project = 164
    )
  )

  expect_true(
    any(
      grepl("receiver_push_summary", list.files(getwd()))
    )
  )
})


test_that("error with no way to find files", {
  expect_error(
    matos_receiver_summary(),
    "Must provide an ACT.*qualified.*unqualified.*deployment"
  )
})
