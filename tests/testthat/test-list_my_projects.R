test_that("passes the sniff test", {
  skip_on_cran()
  skip_on_runiverse()

  all_projects <- list_projects(
    quiet = TRUE, force = TRUE,
    warn_multimatch = FALSE
  )
  my_projects <- list_my_projects(
    force = TRUE,
    warn_multimatch = FALSE
  )

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
  skip_on_cran()
  skip_on_runiverse()

  my_projects <- list_my_projects(warn_multimatch = FALSE)

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




test_that("`read_access = FALSE` works", {
  skip_on_cran()
  skip_on_runiverse()

  # Faster run without read access filtering
  expect_lt(
    system.time({
      no_read <- list_my_projects(read_access = FALSE, warn_multimatch = FALSE)
    })["elapsed"],
    system.time({
      read <- list_my_projects(read_access = TRUE, force = TRUE, warn_multimatch = FALSE)
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




test_that("shushes list_projects under the hood", {
  skip_on_cran()
  skip_on_runiverse()

  expect_no_message(
    list_my_projects(force = TRUE, warn_multimatch = FALSE)
  )
})




test_that("memoise works in theory", {
  # Internal function is memoised
  expect_true(
    memoise::is.memoised(list_my_projects_mem)
  )

  # Clear any previous results
  expect_true(
    memoise::forget(list_my_projects_mem)
  )
})


test_that("memoise works in practice", {
  skip_on_cran()
  skip_on_runiverse()

  # Clear any previous results
  forgotten <- memoise::forget(list_my_projects_mem)

  # First run takes time
  expect_gt(
    time_to_run <- system.time(list_my_projects(warn_multimatch = FALSE))["elapsed"],
    0
  )

  # Creates cache
  expect_true(
    memoise::has_cache(list_my_projects_mem)(
      read_access = TRUE, warn_multimatch = FALSE
    )
  )

  # Next call hits cache
  expect_gt(
    time_to_run,
    time_to_run_cached <- system.time(list_my_projects(warn_multimatch = FALSE))["elapsed"]
  )


  # Forcing works
  expect_gt(
    system.time(list_my_projects(force = TRUE, warn_multimatch = FALSE))["elapsed"],
    time_to_run_cached
  )
})
