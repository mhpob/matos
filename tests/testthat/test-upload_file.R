test_that("errors if trying to provide multiple data types or projects", {
  expect_error(
    upload_file(
      project = c("a", "b"),
      file = "a"
    ),
    "Only able to upload one type of data to one project at a time"
  )

  expect_error(
    upload_file(
      project = "a",
      file = "a",
      data_type = c("new_tags", "receivers")
    ),
    "Only able to upload one type of data to one project at a time"
  )
})


test_that("errors if file can't be found", {
  expect_error(
    upload_file(
      project = "a",
      file = "a",
      data_type = "new_tags"
    ),
    "Unable to find:.*a"
  )
})


test_that("errors if file extension doesn't match data type", {
  dummy_file <- file.path(tempdir(), "dummy.blahblahfile")
  invisible(file.create(dummy_file))


  expect_error(
    upload_file(
      project = "a",
      file = dummy_file,
      data_type = "new_tags"
    ),
    "should be CSV, XLS, or XLSX"
  )

  expect_error(
    upload_file(
      project = "a",
      file = dummy_file,
      data_type = "receivers"
    ),
    "should be CSV, XLS, or XLSX"
  )

  expect_error(
    upload_file(
      project = "a",
      file = dummy_file,
      data_type = "glider"
    ),
    "should be CSV, XLS, or XLSX"
  )

  expect_error(
    upload_file(
      project = "a",
      file = dummy_file,
      data_type = "events"
    ),
    "should be CSV\\."
  )

  expect_error(
    upload_file(
      project = "a",
      file = dummy_file,
      data_type = "gps"
    ),
    "should be CSV\\."
  )

  expect_error(
    upload_file(
      project = "a",
      file = dummy_file,
      data_type = "detections"
    ),
    "should be VRL, VDAT, or CSV\\."
  )

  unlink(dummy_file)
})


test_that("input options haven't changed", {
  # Check the submission drop down and `upload_file` lines 141-152 if this fails

  skip_if_offline()
  skip_on_runiverse()

  url <- "https://matos.asascience.com/report/submit"

  login_check(url)

  site <- httr::GET(url)

  data_types <- httr::content(site)
  data_types <- rvest::html_node(data_types, xpath = "//*[@id=\"selData\"]")
  data_types <- rvest::html_nodes(data_types, "option")
  data_type_id <- rvest::html_attr(data_types, "value")
  data_types <- rvest::html_text(data_types)

  data_types <- data.frame(id = data_type_id[-1], data_type = data_types[-1])

  expect_equal(
    data.frame(
      id = as.character(c(1, 2, 5, 3, 4, 6:9)),
      data_type = c(
        "Tagged Fish - Tag Metadata",
        "Deployed Receivers – Deployment Metadata",
        "Tag Detections - .vrl file",
        "Tag Detections – Unedited Detections .csv ",
        "Tag Detections – Event records .csv",
        "Glider GPS Track - .csv",
        "Glider Deployments - Deployment Metadata - .xlsx ",
        "Tag Vendor Spec Sheets ",
        "Receiver Vendor Spec Sheets"
      )
    ),
    data_types
  )
})
