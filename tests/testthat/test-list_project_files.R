test_that("lists by project code, upper case", {
  skip_if_offline()
  skip_on_runiverse()

  proj_files <- list_project_files("MDWEA")

  expect_s3_class(proj_files, "data.frame")
  expect_named(
    proj_files,
    c("project", "file_type", "upload_date", "file_name", "url")
  )
  expect_type(proj_files$project, "character")
  expect_type(proj_files$file_type, "character")
  expect_s3_class(proj_files$upload_date, "Date")
  expect_type(proj_files$file_name, "character")
  expect_type(proj_files$url, "character")
})

test_that("lists by project code, lower case", {
  skip_if_offline()
  skip_on_runiverse()

  proj_files <- list_project_files("mdwea")

  expect_s3_class(proj_files, "data.frame")
  expect_named(
    proj_files,
    c("project", "file_type", "upload_date", "file_name", "url")
  )
  expect_type(proj_files$project, "character")
  expect_type(proj_files$file_type, "character")
  expect_s3_class(proj_files$upload_date, "Date")
  expect_type(proj_files$file_name, "character")
  expect_type(proj_files$url, "character")
})


test_that("error if no project listed", {
  expect_error(
    list_project_files(),
    "argument \"project\" is missing, with no default"
  )
})


test_that("lists detection files", {
  skip_if_offline()
  skip_on_runiverse()

  detections <- list_project_files(87, file_type = "detections")

  expect_true(
    all(grepl("^Tag Detections.*file$", detections$file_type))
  )
  expect_true(
    all(grepl("\\.(vrl|vdat|csv)$", detections$file_name))
  )
})

test_that("lists receiver metadata", {
  skip_if_offline()
  skip_on_runiverse()

  rec_meta <- list_project_files(87, file_type = "receiver_metadata")

  # Note this uses an em dash!
  expect_true(
    all(rec_meta$file_type == "Deployed Receivers – Deployment Metadata")
  )
  expect_true(
    all(grepl("\\.(xls(x)?|csv)$", rec_meta$file_name))
  )
})


test_that("lists tag metadata", {
  skip_if_offline()
  skip_on_runiverse()

  tag_meta <- list_project_files(87, file_type = "tag_metadata")

  # Note this uses an em dash!
  expect_true(
    all(tag_meta$file_type == "Tagged Fish – Tag Metadata")
  )
  expect_true(
    all(grepl("\\.xls(x)?$", tag_meta$file_name))
  )
})



test_that("gets files since a date", {
  skip_if_offline()
  skip_on_runiverse()

  project_files <- list_project_files(87)

  proj_files_since <- list_project_files(87, since = "2020-06-01")

  expect_true(
    all(proj_files_since$upload_date >= "2020-06-01")
  )

  expect_identical(
    project_files[project_files$upload_date >= "2020-06-01", ],
    proj_files_since
  )
})




test_that("warns with bad \"since\" date", {
  skip_if_offline()
  skip_on_runiverse()

  project_files <- list_project_files(87)

  expect_warning(
    proj_files_since <- list_project_files(87, since = "06/01/2020"),
    "The \"since\" date was not provided in YYYY-MM-DD format\\."
  )

  expect_equal(
    project_files,
    proj_files_since
  )
})
