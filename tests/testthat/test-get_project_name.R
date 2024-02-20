test_that("code matching works", {
  skip_if_offline()
  skip_on_runiverse()

  expect_equal(
    name <- get_project_name(161),
    "UMCES Chesapeake Backbone, Mid-Bay"
  )

  expect_type(name, "character")
})


test_that("errors appropriately", {
  skip_if_offline()
  skip_on_runiverse()

  expect_error(
    get_project_name(8675309),
    "No projects matched with \"8675309\"."
  )
})
