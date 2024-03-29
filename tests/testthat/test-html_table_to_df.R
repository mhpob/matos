test_that("converts data extraction files to df", {
  skip_if_offline()
  skip_on_runiverse()

  html_file_list <- get_file_list(161, "dataextractionfiles")


  expect_s3_class(
    html_df <- html_table_to_df(html_file_list),
    "data.frame"
  )
  expect_named(
    html_df,
    c(
      "project", "file_type", "detection_type", "detection_year", "upload_date",
      "file_name", "url"
    )
  )
})



test_that("converts project files to df", {
  skip_if_offline()
  skip_on_runiverse()

  html_file_list <- get_file_list(161, "downloadfiles")


  expect_s3_class(
    html_df <- html_table_to_df(html_file_list),
    "data.frame"
  )
  expect_named(
    html_df,
    c("project", "file_type", "upload_date", "file_name", "url")
  )
})




test_that("handles projects that have no files", {
  html_file_list <- readRDS(
    test_path("testfiles/html_file_list_nofiles.RDS")
  )

  expect_s3_class(
    html_df <- html_table_to_df(html_file_list),
    "data.frame"
  )

  expect_equal(
    nrow(html_df),
    0
  )

  expect_named(
    html_df,
    c(
      "project", "file_type", "detection_type", "detection_year", "upload_date",
      "file_name", "url"
    )
  )
})
