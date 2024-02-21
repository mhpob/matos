test_that("returns files", {
  skip_if_offline()
  skip_on_runiverse()

  last_match_date <- max(list_extract_files(87)$upload_date)

  td <- file.path(tempdir(), "test-get_extract_updates")
  dir.create(td)

  files <- get_extract_updates(87, since = last_match_date, out_dir = td)
  expect_type(
    files,
    "list"
  )

  expect_type(
    files[[1]],
    "character"
  )

  expect_true(
    all(
      file.exists(
        unlist(files)
      )
    )
  )

  # Clean up
  unlink(td, recursive = TRUE)
})



test_that("errors with no project provided", {
  expect_error(
    get_extract_updates(),
    "argument \"project\" is missing, with no default"
  )
})

test_that("warns with no \"since\" date", {
  skip_if_offline()
  skip_on_runiverse()

  td <- file.path(tempdir(), "test-get_extract_updates")
  dir.create(td)

  expect_message(
    get_extract_updates(87, out_dir = td),
    "No \"since\" date has been provided\\."
  ) |>
    expect_message("All extract files will be downloaded!")

  unlink(td, recursive = TRUE)
})


test_that("informative message with no new files", {
  skip_if_offline()
  skip_on_runiverse()

  expect_message(
    get_extract_updates(87, since = Sys.Date() + 2),
    "No files uploaded since the provided date\\."
  )
})


test_that("can be shushed", {
  skip_if_offline()
  skip_on_runiverse()

  last_match_date <- max(list_extract_files(87)$upload_date)

  td <- file.path(tempdir(), "test-get_extract_updates")
  dir.create(td)

  expect_no_message(
    get_extract_updates(87, since = last_match_date, progress = FALSE, out_dir = td)
  )

  unlink(td, recursive = TRUE)
})
