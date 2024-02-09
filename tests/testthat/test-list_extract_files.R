test_that("error if no project listed", {
  skip_on_cran()

  expect_error(
    list_extract_files()
  )
})

test_that("no error in listing extract files", {
  skip_on_cran()

  expect_no_error(
    list_extract_files(87)
  )
})


test_that("project with no files returns empty data frame", {
  skip_on_cran()

  empty <- list_extract_files(160)

  expect_s3_class(empty, "data.frame")
  expect_equal(nrow(empty), 0)
  expect_equal(ncol(empty), 7)
})

test_that("returns correct classes", {
  skip_on_cran()

  extract_files <- list_extract_files(87)

  expect_s3_class(extract_files, "data.frame")
  expect_type(extract_files$project, "character")
  expect_equal(class(extract_files$upload_date), "Date")
  expect_type(extract_files$url, "character")
})



test_that("lists matched files", {
  skip_on_cran()

  matched_files <- list_extract_files(87, detection_type = "matched")

  expect_equal(unique(matched_files$detection_type), "matched")
})
test_that("lists external partner matches", {
  skip_on_cran()

  external_files <- list_extract_files(87, detection_type = "external")

  expect_equal(unique(external_files$detection_type), "matched_external_partners")
})
test_that("lists qualified detections", {
  skip_on_cran()

  qualified_files <- list_extract_files(87, detection_type = "qualified")

  expect_equal(unique(qualified_files$detection_type), "qualified")
})
test_that("lists sentinel detections", {
  skip_on_cran()

  sentinel_files <- list_extract_files(87, detection_type = "sentinel")

  expect_equal(unique(sentinel_files$detection_type), "sentinel_tag")
})
test_that("lists unqualified detections", {
  skip_on_cran()

  unqualified_files <- list_extract_files(87, detection_type = "unqualified")

  expect_equal(unique(unqualified_files$detection_type), "unqualified")
})
