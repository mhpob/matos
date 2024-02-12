test_that("errors with incorrect project", {
  expect_error(
    get_file_list(8675309, "dataextractionfiles"),
    "No file lists returned\\."
  )
})


test_that("returns correct class", {
  skip_on_cran()

  expect_s3_class(
    file_list <- get_file_list(161, "dataextractionfiles"),
    "response"
  )

  expect_s3_class(
    httr::content(file_list),
    c("xml_document", "xml_node"),
    exact = TRUE
  )
})


test_that("contains a table", {
  skip_on_cran()

  tab_attr <- get_file_list(161, "dataextractionfiles") |>
    httr::content() |>
    rvest::html_element(xpath = '//*[@id="content"]/table') |>
    rvest::html_attr("class")

  expect_equal(
    tab_attr,
    "tableContent"
  )
})


test_that("memoisation works in general", {
  # Make sure function is memoised
  expect_true(
    memoise::is.memoised(get_file_list_mem)
  )

  # Clear any previous results
  expect_true(
    memoise::forget(get_file_list_mem)
  )
})


test_that("memoisation works on correct projects", {
  skip_on_cran()

  # Clear any previous results
  forgotten <- memoise::forget(get_file_list_mem)

  # First run takes time
  expect_gt(
    time_to_run <- system.time(get_file_list(161, "dataextractionfiles"))["elapsed"],
    0
  )

  # Creates cache
  expect_true(
    memoise::has_cache(get_file_list_mem)(161, "dataextractionfiles")
  )

  # Next call hits cache
  expect_gt(
    time_to_run,
    time_to_run_cached <- system.time(
      get_file_list(161, "dataextractionfiles")
      )["elapsed"]
  )


  # Forcing works
  expect_gt(
    system.time(get_file_list(161, "dataextractionfiles", force = T))["elapsed"],
    time_to_run_cached
  )
})
