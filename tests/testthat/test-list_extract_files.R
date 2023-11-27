test_that("error if no project listed", {
  # skip test on Runiverse
  skip_on_Runiverse()

  expect_error(
    list_extract_files()
  )
})

test_that("no error in listing extract files", {
  # skip test on Runiverse
  skip_on_Runiverse()

  expect_no_error(
    list_extract_files(87)
  )
})


test_that("project with no files returns empty data frame", {
  # skip test on Runiverse
  skip_on_Runiverse()

  empty <- list_extract_files(160)

  expect_s3_class(empty, "data.frame")
  expect_equal(nrow(empty), 0)
  expect_equal(ncol(empty), 7)
})

test_that("returns correct classes", {
  # skip test on Runiverse
  skip_on_Runiverse()

  extract_files <- list_extract_files(87)

  expect_s3_class(extract_files, "data.frame")
  expect_type(extract_files$project, "character")
  expect_equal(class(extract_files$upload_date), "Date")
  expect_type(extract_files$url, "character")
})



test_that("lists matched files", {
  # skip test on Runiverse
  skip_on_Runiverse()

  matched_files <- list_extract_files(87, detection_type = "matched")

  expect_equal(unique(matched_files$detection_type), "matched")
})
test_that("lists external partner matches", {
  # skip test on Runiverse
  skip_on_Runiverse()

  external_files <- list_extract_files(87, detection_type = "external")

  expect_equal(unique(external_files$detection_type), "matched_external_partners")
})
test_that("lists qualified detections", {
  # skip test on Runiverse
  skip_on_Runiverse()

  qualified_files <- list_extract_files(87, detection_type = "qualified")

  expect_equal(unique(qualified_files$detection_type), "qualified")
})
test_that("lists sentinel detections", {
  # skip test on Runiverse
  skip_on_Runiverse()

  sentinel_files <- list_extract_files(87, detection_type = "sentinel")

  expect_equal(unique(sentinel_files$detection_type), "sentinel_tag")
})
test_that("lists unqualified detections", {
  # skip test on Runiverse
  skip_on_Runiverse()

  unqualified_files <- list_extract_files(87, detection_type = "unqualified")

  expect_equal(unique(unqualified_files$detection_type), "unqualified")
})
