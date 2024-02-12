test_that("downloads with url", {
  skip_on_cran()
  skip_on_runiverse()

  login_check()

  td <- file.path(tempdir(), 'tests')
  dir.create(td)

  expect_message(
    download_process(
      'https://matos.asascience.com/projectfile/downloadExtraction/161_1',
      out_dir = td,
      overwrite = FALSE,
      to_vue = FALSE
    ),
    "Downloading files"
  ) |>
    expect_message("File\\(s\\) saved to") |>
    expect_message("Unzipping files") |>
    expect_message("File\\(s\\) unzipped to")

  expect_true(
    any(
      grepl(
        "cbbbmb_qualified_detections.*csv$",
        list.files(td)
      )
    )
  )
  expect_true(
    any(
      grepl(
        "cbbbmb_qualified_detections.*zip$",
        list.files(td)
      )
    )
  )
  expect_true(
    any(
      grepl(
        "^data_description\\.txt$",
        list.files(td)
      )
    )
  )

  unlink(td, recursive = TRUE)
})


test_that("overwrite works", {
  skip_on_cran()
  skip_on_runiverse()

  login_check()

  td <- file.path(tempdir(), 'tests')
  dir.create(td)

  suppressMessages(
    download_process(
      'https://matos.asascience.com/projectfile/downloadExtraction/161_1',
      out_dir = td,
      overwrite = FALSE,
      to_vue = FALSE
    )
  )

  expect_error(
    suppressMessages(
      download_process(
        'https://matos.asascience.com/projectfile/downloadExtraction/161_1',
        out_dir = td,
        overwrite = FALSE,
        to_vue = FALSE
      )
    ),
    "Path exists and overwrite is FALSE"
  )

  # shouldn't error
  suppressMessages(
    download_process(
      'https://matos.asascience.com/projectfile/downloadExtraction/161_1',
      out_dir = td,
      overwrite = TRUE,
      to_vue = FALSE
    )
  )


  unlink(td, recursive = TRUE)
})


test_that("returns file paths", {
  skip_on_cran()
  skip_on_runiverse()

  login_check()

  td <- file.path(tempdir(), 'tests')
  dir.create(td)

  suppressMessages(
    file_paths <- download_process(
      'https://matos.asascience.com/projectfile/downloadExtraction/161_1',
      out_dir = td,
      overwrite = FALSE,
      to_vue = FALSE
    )
  )

  expect_type(file_paths, "character")
  expect_true(all(file.exists(file_paths)))


  unlink(td, recursive = TRUE)
})
