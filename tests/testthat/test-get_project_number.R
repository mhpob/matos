test_that("direct full name matching works", {
  expect_equal(
    proj_num <- get_project_number("UMCES BOEM Marine Mammal Monitoring"),
    242
  )

  expect_type(proj_num, 'double')
})




test_that("code matching works", {
  expect_equal(
    proj_num <- get_project_number("MDWEA"),
    87
  )

  expect_type(proj_num, 'double')

})



test_that("mixed case matching works", {
  expect_equal(
    proj_num <- get_project_number("umces BOEM Marine Mammal Monitoring"),
    242
  )

  expect_equal(
    proj_num <- get_project_number("mdwea"),
    87
  )
})


test_that("errors correctly", {
  # errors and suggests possible matches if agrep finds something
  expect_message(
    get_project_number("umces"),
    "Perhaps you meant one of the following:"
    ) |>
    expect_error("No projects matched with \"umces\"")

  # no message if not
  expect_error(
    get_project_number("blahblahblah"),
    "No projects matched with \"blahblahblah\""
  ) |>
    expect_no_message()
})
