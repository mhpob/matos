test_that("passes the sniff test", {
  all_projects <- list_projects(quiet = TRUE, force = TRUE)
  my_projects <- list_my_projects(force = TRUE)

  # Read-access projects is a subset of all projects
  expect_contains(
    all_projects$name,
    my_projects$name
  )

  # There are fewer read-access projects than total projects
  expect_lt(
    nrow(my_projects),
    nrow(all_projects)
  )
})



test_that("has correct classes", {
  my_projects <- list_my_projects()

  expect_s3_class(my_projects, "data.frame")
  expect_type(my_projects$name, "character")
  expect_type(my_projects$collectioncode, "character")
  expect_type(my_projects$number, "double")
  expect_type(my_projects$url, "character")
  expect_named(
    my_projects,
    c(
      "name", "collectioncode", "number", "url", "status", "longname", "citation",
      "website", "collaborationtype", "locality", "abstract"
    )
  )
})




test_that("shushes list_projects under the hood", {
  expect_no_message(
    list_my_projects(force = TRUE)
  )
})




test_that("memoise works", {
  # Internal unction is memoised
  expect_true(
    memoise::is.memoised(list_my_projects_mem)
  )

  # Takes longer to run the forced function than the memoised function
  expect_gt(
    system.time(list_my_projects(force = TRUE))["elapsed"],
    system.time(list_my_projects())["elapsed"]
  )
})




test_that("`read_access = FALSE` works", {
  # Faster run without read access filtering
  expect_lt(
    system.time({
      no_read <- list_my_projects(read_access = FALSE)
      })["elapsed"],
    system.time({
      read <- list_my_projects(read_access = TRUE, force = TRUE)
      })["elapsed"]
  )

  # Read-access projects is a subset of no-read-access projects
  expect_contains(
    no_read$name,
    read$name
  )

  # There are fewer read-access projects than no-read projects
  expect_lt(
    nrow(read),
    nrow(no_read)
  )
})
