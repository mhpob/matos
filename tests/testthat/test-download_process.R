setup_dls <- function(x) {
  login_check()

  td <- file.path(tempdir(), "tests")
  dir.create(td)

  list_extract_files(x)
}

test_that("downloads qualified detections with url", {
  skip_on_cran()
  skip_on_runiverse()

  extract_files <- setup_dls(87)
  td <- file.path(tempdir(), "tests")

  expect_message(
    download_process(
      extract_files[extract_files$detection_type == "qualified", "url"][1],
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
        "qualified_detections.*csv$",
        list.files(td)
      )
    )
  )
  expect_true(
    any(
      grepl(
        "qualified_detections.*zip$",
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

test_that("downloads unqualified detections with url", {
  skip_on_cran()
  skip_on_runiverse()

  extract_files <- setup_dls(87)
  td <- file.path(tempdir(), "tests")

  expect_message(
    download_process(
      extract_files[extract_files$detection_type == "unqualified", "url"][1],
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
        "unqualified_detections.*csv$",
        list.files(td)
      )
    )
  )
  expect_true(
    any(
      grepl(
        "unqualified_detections.*zip$",
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

test_that("downloads matched detections with url", {
  skip_on_cran()
  skip_on_runiverse()

  extract_files <- setup_dls(87)
  td <- file.path(tempdir(), "tests")

  expect_message(
    download_process(
      extract_files[extract_files$detection_type == "matched", "url"][1],
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
        "matched_detections.*csv$",
        list.files(td)
      )
    )
  )
  expect_true(
    any(
      grepl(
        "matched_detections.*zip$",
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

test_that("downloads external partners detections with url", {
  skip_on_cran()
  skip_on_runiverse()

  extract_files <- setup_dls(87)
  td <- file.path(tempdir(), "tests")

  expect_message(
    download_process(
      extract_files[
        extract_files$detection_type == "matched_external_partners", "url"
      ][1],
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
        "matched_external_partners_detections.*csv$",
        list.files(td)
      )
    )
  )
  expect_true(
    any(
      grepl(
        "matched_external_partners_detections.*zip$",
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

test_that("downloads sentinel detections with url", {
  skip_on_cran()
  skip_on_runiverse()

  extract_files <- setup_dls(87)
  td <- file.path(tempdir(), "tests")

  expect_message(
    download_process(
      extract_files[
        extract_files$detection_type == "sentinel_tag", "url"
      ][1],
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
        "sentinel_tag_detections.*csv$",
        list.files(td)
      )
    )
  )
  expect_true(
    any(
      grepl(
        "sentinel_tag_detections.*zip$",
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

  extract_files <- setup_dls(87)
  td <- file.path(tempdir(), "tests")

  suppressMessages(
    download_process(
      extract_files$url[1],
      out_dir = td,
      overwrite = FALSE,
      to_vue = FALSE
    )
  )

  expect_error(
    suppressMessages(
      download_process(
        extract_files$url[1],
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
      extract_files$url[1],
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

  extract_files <- setup_dls(87)
  td <- file.path(tempdir(), "tests")

  suppressMessages(
    file_paths <- download_process(
      extract_files$url[1],
      out_dir = td,
      overwrite = FALSE,
      to_vue = FALSE
    )
  )

  expect_type(file_paths, "character")
  expect_true(all(file.exists(file_paths)))


  unlink(td, recursive = TRUE)
})


test_that("to_vue: qualified", {
  skip_on_cran()
  skip_on_runiverse()

  extract_files <- setup_dls(87)
  td <- file.path(tempdir(), "tests")

  expect_message(
    file_paths <- download_process(
      extract_files[extract_files$detection_type == "qualified", "url"][1],
      out_dir = td,
      overwrite = TRUE,
      to_vue = TRUE
    ),
    "Downloading files"
  ) |>
    expect_message("Unzipping files") |>
    expect_message("Converting to VUE CSV format") |>
    expect_message("CSV converted to VUE format")

  expect_true(
    any(grepl("^vue.*.csv$", basename(file_paths)))
  )

  vue_file <- read.csv(
    grep("/vue.*.csv$", file_paths, value = TRUE),
    check.names = F
  )

  expect_named(
    vue_file,
    c(
      "Date and Time (UTC)", "Receiver", "Transmitter",
      "Transmitter Name", "Transmitter Serial", "Sensor Value",
      "Sensor Unit", "Station Name", "Latitude", "Longitude"
    )
  )


  unlink(td, recursive = TRUE)
})


test_that("to_vue: unqualified", {
  skip_on_cran()
  skip_on_runiverse()

  extract_files <- setup_dls(87)
  td <- file.path(tempdir(), "tests")

  expect_message(
    file_paths <- download_process(
      extract_files[extract_files$detection_type == "unqualified", "url"][1],
      out_dir = td,
      overwrite = TRUE,
      to_vue = TRUE
    ),
    "Downloading files"
  ) |>
    expect_message("Unzipping files") |>
    expect_message("Converting to VUE CSV format") |>
    expect_message("CSV converted to VUE format")

  expect_true(
    any(grepl("^vue.*.csv$", basename(file_paths)))
  )

  vue_file <- read.csv(
    grep("/vue.*.csv$", file_paths, value = TRUE),
    check.names = F
  )

  expect_named(
    vue_file,
    c(
      "Date and Time (UTC)", "Receiver", "Transmitter",
      "Transmitter Name", "Transmitter Serial", "Sensor Value",
      "Sensor Unit", "Station Name", "Latitude", "Longitude"
    )
  )

  unlink(td, recursive = TRUE)
})


test_that("to_vue: matched", {
  skip_on_cran()
  skip_on_runiverse()

  extract_files <- setup_dls(87)
  td <- file.path(tempdir(), "tests")

  expect_message(
    file_paths <- download_process(
      extract_files[extract_files$detection_type == "matched", "url"][1],
      out_dir = td,
      overwrite = TRUE,
      to_vue = TRUE
    ),
    "Downloading files"
  ) |>
    expect_message("Unzipping files") |>
    expect_message("Converting to VUE CSV format") |>
    expect_message("CSV converted to VUE format")

  expect_true(
    any(grepl("^vue.*.csv$", basename(file_paths)))
  )

  vue_file <- read.csv(
    grep("/vue.*.csv$", file_paths, value = TRUE),
    check.names = F
  )

  expect_named(
    vue_file,
    c(
      "Date and Time (UTC)", "Receiver", "Transmitter",
      "Transmitter Name", "Transmitter Serial", "Sensor Value",
      "Sensor Unit", "Station Name", "Latitude", "Longitude"
    )
  )


  unlink(td, recursive = TRUE)
})


test_that("to_vue: sentinel", {
  skip_on_cran()
  skip_on_runiverse()

  extract_files <- setup_dls(87)
  td <- file.path(tempdir(), "tests")

  expect_message(
    file_paths <- download_process(
      extract_files[extract_files$detection_type == "sentinel_tag", "url"][1],
      out_dir = td,
      overwrite = TRUE,
      to_vue = TRUE
    ),
    "Downloading files"
  ) |>
    expect_message("Unzipping files") |>
    expect_message("Converting to VUE CSV format") |>
    expect_message("CSV converted to VUE format")

  expect_true(
    any(grepl("^vue.*.csv$", basename(file_paths)))
  )

  vue_file <- read.csv(
    grep("/vue.*.csv$", file_paths, value = TRUE),
    check.names = F
  )

  expect_named(
    vue_file,
    c(
      "Date and Time (UTC)", "Receiver", "Transmitter",
      "Transmitter Name", "Transmitter Serial", "Sensor Value",
      "Sensor Unit", "Station Name", "Latitude", "Longitude"
    )
  )


  unlink(td, recursive = TRUE)
})


test_that("to_vue: external partners warns", {
  skip_on_cran()
  skip_on_runiverse()

  extract_files <- setup_dls(87)
  td <- file.path(tempdir(), "tests")


  expect_message(
    file_paths <- download_process(
      extract_files[
        extract_files$detection_type == "matched_external_partners", "url"
      ][1],
      out_dir = td,
      overwrite = TRUE,
      to_vue = TRUE
    ),
    "Downloading files"
  ) |>
    expect_message("Unzipping files") |>
    expect_warning("Conversion to VUE CSV format will not take place")

  expect_false(
    any(grepl("^vue.*.csv$", basename(file_paths)))
  )
})
