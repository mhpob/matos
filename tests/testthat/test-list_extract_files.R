test_that("error if no project listed", {
  skip_if_offline()
  skip_on_runiverse()

  expect_error(
    list_extract_files(),
    "No file lists returned."
  )
})



test_that("project with no files returns empty data frame", {
  skip_on_cran()
  skip_on_runiverse()

  empty <- list_extract_files(160)

  expect_s3_class(empty, "data.frame")
  expect_equal(nrow(empty), 0)
  expect_equal(ncol(empty), 7)
})



test_that("returns correct classes", {
  skip_on_cran()
  skip_on_runiverse()

  extract_files <- list_extract_files(87)

  expect_s3_class(extract_files, "data.frame")
  expect_type(extract_files$project, "character")
  expect_equal(class(extract_files$upload_date), "Date")
  expect_type(extract_files$url, "character")
})



test_that("lists matched files", {
  skip_on_cran()
  skip_on_runiverse()

  matched_files <- list_extract_files(87, detection_type = "matched")

  expect_equal(unique(matched_files$detection_type), "matched")

  expect_true(
    all(grepl("_matched_detections_\\d{4}.zip$", matched_files$file_name))
  )
})



test_that("lists external partner matches", {
  skip_on_cran()
  skip_on_runiverse()

  external_files <- list_extract_files(87, detection_type = "external")

  expect_equal(
    unique(external_files$detection_type),
    "matched_external_partners"
  )
  expect_true(
    all(
      grepl(
        "matched_external_partners_detections_\\d{4}.zip$",
        external_files$file_name
      )
    )
  )
})



test_that("lists qualified detections", {
  skip_on_cran()
  skip_on_runiverse()

  qualified_files <- list_extract_files(87, detection_type = "qualified")

  expect_equal(unique(qualified_files$detection_type), "qualified")
  expect_true(
    all(
      grepl(
        "qualified_detections_\\d{4}.zip$",
        qualified_files$file_name
      )
    )
  )
})


test_that("lists sentinel detections", {
  skip_on_cran()
  skip_on_runiverse()

  sentinel_files <- list_extract_files(87, detection_type = "sentinel")

  expect_equal(unique(sentinel_files$detection_type), "sentinel_tag")
  expect_true(
    all(
      grepl(
        "sentinel_tag_detections_\\d{4}.zip$",
        sentinel_files$file_name
      )
    )
  )
})


test_that("lists unqualified detections", {
  skip_on_cran()
  skip_on_runiverse()

  unqualified_files <- list_extract_files(87, detection_type = "unqualified")

  expect_equal(unique(unqualified_files$detection_type), "unqualified")
  expect_true(
    all(
      grepl(
        "unqualified_detections_\\d{4}.zip$",
        unqualified_files$file_name
      )
    )
  )
})




test_that("gets files since a date", {
  skip_if_offline()
  skip_on_runiverse()

  extract_files <- list_extract_files(87)

  extracts_since <- list_extract_files(87, since = "2023-08-01")

  expect_true(
    all(extracts_since$upload_date >= "2023-08-01")
  )

  expect_identical(
    extract_files[extract_files$upload_date >= "2023-08-01", ],
    extracts_since
  )
})




test_that("warns with bad \"since\" date", {
  skip_if_offline()
  skip_on_runiverse()

  extract_files <- list_extract_files(87)

  expect_warning(
    extracts_since <- list_extract_files(87, since = "08/01/2023"),
    "The \"since\" date was not provided in YYYY-MM-DD format\\."
  )

  expect_equal(
    extract_files,
    extracts_since
  )
})
