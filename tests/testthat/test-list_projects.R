test_that("returns correct classes", {
  # skip test on Runiverse
  skip_on_Runiverse()

  projects <- list_projects()

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




#### NON-EXPORTED FUNCTIONS ####
test_that("internal function `flatten_names` works", {
  otn_names <- paste0(
    "https://members.oceantrack.org/geoserver/otn/ows?service=WFS&",
    "version=1.0.0&request=GetFeature&typeName=otn:",
    "otn_resources_metadata",
    "&outputFormat=csv&CQL_FILTER=strMatches(node,'ACT')=true"
  ) |>
    URLencode() |>
    read.csv() |>
    _$shortname

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
