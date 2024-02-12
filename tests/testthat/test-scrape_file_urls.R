test_that("returns correct class", {
  skip_on_cran()
  skip_on_runiverse()

  html_file_list <- get_file_list(161, "dataextractionfiles")

  urls <- scrape_file_urls(html_file_list)

  expect_type(
    urls,
    'character'
  )
})


test_that("URLs were built correctly", {
  skip_on_cran()
  skip_on_runiverse()

  html_file_list <- get_file_list(161, "dataextractionfiles")

  urls <- scrape_file_urls(html_file_list)

  expect_true(
    all(
      grepl(
        "https://matos.asascience.com/projectfile/downloadExtraction",
        dirname(urls)
      )
    )
  )
  expect_true(
    all(
      grepl(
        '161_',
        basename(urls)
      )
    )
  )
})
