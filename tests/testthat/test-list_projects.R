test_that("returns correct classes", {
  skip_if_offline()
  skip_on_runiverse()

  projects <- list_projects(quiet = TRUE)

  expect_s3_class(projects, "data.frame")
  expect_type(projects$name, "character")
  expect_type(projects$collectioncode, "character")
  expect_type(projects$number, "double")
  expect_type(projects$url, "character")
  expect_named(
    projects,
    c(
      "name", "collectioncode", "number", "url", "status", "longname", "citation",
      "website", "collaborationtype", "locality", "abstract"
    )
  )
})




test_that("passing \"mine\" to \"what\" argument works", {
  skip_on_cran()
  skip_on_runiverse()

  projects <- list_projects(quiet = T)
  my_projects <- list_projects(what = "mine")
  my_projects_no_read <- list_projects(what = "mine", read_access = FALSE)


  # Read-access projects is a subset of all projects
  expect_contains(
    projects$name,
    my_projects$name
  )

  # No read access is a subset of all projects
  expect_contains(
    projects$name,
    my_projects_no_read$name
  )
  # Read access is a subset of no read access
  expect_contains(
    my_projects_no_read$name,
    my_projects$name
  )

  # There are fewer no-read-access projects than total projects
  expect_lt(
    nrow(my_projects_no_read),
    nrow(projects)
  )
  # There are fewer read-access projects than no-read-access projects
  expect_lt(
    nrow(my_projects),
    nrow(my_projects_no_read)
  )
})




test_that("can shush", {
  skip_if_offline()
  skip_on_runiverse()

  expect_message(
    list_projects(force = T),
    "These ACT projects were unable to be matched with OTN"
  ) |>
    expect_message("These OTN projects were unable to be matched with ACT")

  expect_no_message(
    list_projects(quiet = TRUE, force = TRUE)
  )
})




test_that("memoise works in theory", {

  # Internal function is memoised
  expect_true(
    memoise::is.memoised(list_projects_mem)
  )

  # Clear any previous results
  expect_true(
    memoise::forget(list_projects_mem)
  )
})

test_that("memoise works in practice", {
  skip_if_offline()

  # Pings the server and returns a message
  expect_message(
    time_to_run <- system.time(
      projects <- list_projects()
    ),
    "These ACT projects were unable to be matched with OTN"
  ) |>
    expect_message(
      "These OTN projects were unable to be matched with ACT"
    )

  # Creates a cache
  expect_true(
    memoise::has_cache(list_projects_mem)(
      what = "all",
      read_access = TRUE,
      quiet = FALSE
    )
  )

  # Returns cached result (as indicated by not returning the message)
  expect_no_message(
    time_to_run_cached <- system.time(
      projects <- list_projects()
    )
  )

  # Takes longer to run the forced function than the memoised function
  expect_gt(
    time_to_run["elapsed"],
    time_to_run_cached["elapsed"]
  )

  # `force=T` pings server and returns message
  expect_message(
    time_to_run_forced <- system.time(
      projects <- list_projects(force = T)
    ),
    "These ACT projects were unable to be matched with OTN"
  ) |>
    expect_message(
      "These OTN projects were unable to be matched with ACT"
    )

  # Takes longer to run with `force=T` than it did on the cached version
  expect_gt(
    time_to_run_forced["elapsed"],
    time_to_run_cached["elapsed"]
  )
})




#### NON-EXPORTED FUNCTIONS ####
test_that("internal function `flatten_names` works", {
  otn_names <- paste0(
    "https://members.oceantrack.org/geoserver/otn/ows?service=WFS&",
    "version=1.0.0&request=GetFeature&typeName=otn:",
    "otn_resources_metadata",
    "&outputFormat=csv&CQL_FILTER=strMatches(node,'ACT')=true"
  ) |>
    URLencode() |>
    read.csv()
  otn_names <- otn_names$shortname

  flat_names <- flatten_names(otn_names)

  # Right type
  expect_type(flat_names, "character")
  # Right length
  expect_equal(length(otn_names), length(flat_names))
  # No missing
  expect_true(
    !any(is.na(flat_names))
  )
  # No upper case
  expect_true(
    !any(grepl("[A-Z]", flat_names))
  )
  # No special characters
  expect_true(
    !any(
      grepl(
        "[\\$\\&\\+\\,\\:\\;\\=\\?\\@\\#\\|\\'\\<\\>\\.\\^\\*\\(\\)\\%\\!\\-]",
        flat_names
      )
    )
  )
  # No spaces, underscores, or parentheses
  expect_true(
    !any(grepl("[ _\\(\\)]", flat_names))
  )
})




test_that("internal function `within_match` works", {
  projects_db1 <- c(
    "abcdefg",
    "hijklmn"
  )
  projects_db2_onematch <- c(
    "12345",
    "jkl",
    "def"
  )
  projects_db2_multimatch <- c(
    "abcd",
    "defgh",
    "cde"
  )

  expect_type(
    matches <- within_match(projects_db2_onematch, projects_db1),
    "list"
  )
  expect_type(matches[[1]], "character")
  expect_type(matches[[2]], "character")
  expect_length(matches, 2)
  expect_named(matches, c("jkl", "def"))

  expect_error(
    within_match(projects_db2_multimatch, projects_db1),
    "At least one project name is a subset of multiple other projects."
  )
})




test_that("internal function `fuzzy_match` works", {
  projects_db1 <- c(
    "abcdefg",
    "hijklmn"
  )
  projects_db2_onematch <- c(
    "12345",
    "jkl",
    "def"
  )
  projects_db2_multimatch <- c(
    "abcd",
    "defgh",
    "123"
  )

  expect_type(
    matches <- fuzzy_match(projects_db2_onematch, projects_db1),
    "list"
  )
  expect_type(matches[[1]], "character")
  expect_type(matches[[2]], "character")
  expect_length(matches, 2)
  expect_named(matches, c("jkl", "def"))

  expect_error(
    fuzzy_match(projects_db2_multimatch, projects_db1),
    "At least one project name can be fuzzy-matched to multiple other projects."
  )
})
