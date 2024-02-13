test_that("gets file by URL", {
  skip_if_offline()
  skip_on_runiverse()

  td <- file.path(tempdir(), "test-get_project_file")
  dir.create(td)

  project_files <- list_project_files(161)

  expect_message(
    file_paths <- get_project_file(
      url = project_files$url[1],
      out_dir = td
    ),
    "Downloading files"
  ) |>
    expect_message("File\\(s\\) saved to")

  expect_type(file_paths, "character")
  expect_true(all(file.exists(file_paths)))

  unlink(td, recursive = T)
})


test_that("gets file by index and project number", {
  skip_if_offline()
  skip_on_runiverse()

  td <- file.path(tempdir(), "test-get_project_file")
  dir.create(td)

  expect_message(
    file_paths <- get_project_file(
      file = 1,
      project = 161,
      out_dir = td
    ),
    "Downloading files"
  ) |>
    expect_message("File\\(s\\) saved to")

  expect_type(file_paths, "character")
  expect_true(all(file.exists(file_paths)))

  unlink(td, recursive = T)
})


test_that("gets file by name and project number", {
  skip_if_offline()
  skip_on_runiverse()

  td <- file.path(tempdir(), "test-get_project_file")
  dir.create(td)

  expect_message(
    file_paths <- get_project_file(
      file = "VR2AR_546323_20231012_1.vrl",
      project = 161,
      out_dir = td
    ),
    "Downloading files"
  ) |>
    expect_message("File\\(s\\) saved to")

  expect_type(file_paths, "character")
  expect_true(all(file.exists(file_paths)))

  unlink(td, recursive = T)
})


test_that("gets file by index and project name", {
  skip_if_offline()
  skip_on_runiverse()

  td <- file.path(tempdir(), "test-get_project_file")
  dir.create(td)

  expect_message(
    file_paths <- get_project_file(
      file = 4,
      project = "UMCES Chesapeake Backbone, Mid-Bay",
      out_dir = td
    ),
    "Downloading files"
  ) |>
    expect_message("File\\(s\\) saved to")

  expect_type(file_paths, "character")
  expect_true(all(file.exists(file_paths)))

  unlink(td, recursive = T)
})


test_that("gets file by name and project name", {
  skip_if_offline()
  skip_on_runiverse()

  td <- file.path(tempdir(), "test-get_project_file")
  dir.create(td)

  expect_message(
    file_paths <- get_project_file(
      file = "VR2AR_546323_20231012_1.vrl",
      project = "UMCES Chesapeake Backbone, Mid-Bay",
      out_dir = td
    ),
    "Downloading files"
  ) |>
    expect_message("File\\(s\\) saved to")

  expect_type(file_paths, "character")
  expect_true(all(file.exists(file_paths)))

  unlink(td, recursive = T)
})


test_that("overwrite works", {
  skip_if_offline()
  skip_on_runiverse()

  td <- file.path(tempdir(), "test-get_project_file")
  dir.create(td)

  suppressMessages(
    file_paths <- get_project_file(
      file = "VR2AR_546323_20231012_1.vrl",
      project = "UMCES Chesapeake Backbone, Mid-Bay",
      out_dir = td
    )
  )

  # Shouldn't error
  suppressMessages(
    file_paths <- get_project_file(
      file = "VR2AR_546323_20231012_1.vrl",
      project = "UMCES Chesapeake Backbone, Mid-Bay",
      out_dir = td,
      overwrite = TRUE
    )
  )

  suppressMessages(
    expect_error(
      get_project_file(
        file = "VR2AR_546323_20231012_1.vrl",
        project = "UMCES Chesapeake Backbone, Mid-Bay",
        out_dir = td,
        overwrite = FALSE
      ),
      "Path exists and overwrite is FALSE"
    )
  )


  unlink(td, recursive = T)
})


test_that("errors if no URL and file and project aren't provided", {
  expect_error(
    get_project_file(),
    "Need a file name/index and its project name/number\\."
  )
})


test_that("errors if the index doesn't exist", {
  expect_error(
    get_project_file(file = 90000, project = 161),
    "There is no index matching what you have provided\\."
  )
})


test_that("errors if it can't find the file name", {
  expect_error(
    get_project_file(file = "blahblah", project = 161),
    "There is no file matching what you have provided"
  )
})


test_that("errors if multiple files are requested", {
  expect_error(
    get_project_file(file = c(1, 2), project = 161),
    "Only one file.*Try looping"
  )

  expect_error(
    get_project_file(
      file = c(
        "cbbbmb_qualified_detections_2021.zip",
        "cbbbmb_qualified_detections_2022.zip"
      ),
      project = 161
    ),
    "Only one file.*Try looping"
  )

  expect_error(
    get_project_file(
      url = c(
        "https://matos.asascience.com/projectfile/downloadExtraction/161_6",
        "https://matos.asascience.com/projectfile/downloadExtraction/161_8"
      )
    ),
    "Only one file.*Try looping"
  )
})
