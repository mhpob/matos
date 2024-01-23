test_that("errors with incorrect project", {
  expect_error(
    get_file_list(8675309, 'dataextractionfiles'),
    'No file lists returned\\.'
  )
})


test_that("returns correct class", {
  skip_on_Runiverse()

  expect_s3_class(
    file_list <- get_file_list(161, 'dataextractionfiles'),
    'response'
  )

  expect_s3_class(
    httr::content(file_list),
    c('xml_document', 'xml_node'),
    exact = TRUE
  )

})


test_that("contains a table", {
  skip_on_Runiverse()

  tab_attr <- get_file_list(161, 'dataextractionfiles') |>
    httr::content() |>
    rvest::html_element(xpath = '//*[@id="content"]/table') |>
    rvest::html_attr('class')

  expect_equal(
    tab_attr,
    'tableContent'
  )

})
