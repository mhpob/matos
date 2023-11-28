flatten_names <- function(x){
  hold <- tolower(x)
  gsub("[,\\(\\)_ /:'&\\.]|-", '', hold)
}
fuzzy_match_fun <- function(a, b){
  hold <- sapply(a, agrep, b,
                 max.distance = 0.25,
                 value = TRUE,
                 ignore.case = TRUE)
  hold[sapply(hold, length) > 0]
}


matos_projects <- httr::GET(
  "https://matos.asascience.com/project"
)

matos_projects <- httr::content(matos_projects)
matos_projects <- rvest::html_node(matos_projects, ".project_list")
matos_projects <- rvest::html_nodes(matos_projects, "a")
matos_projects <- rvest::html_text(matos_projects, trim = T)

matos_projects <- data.frame(
  names = flatten_names(matos_projects)
)

otn_metadata <- paste0(
  "https://members.oceantrack.org/geoserver/otn/ows?service=WFS&",
  "version=1.0.0&request=GetFeature&typeName=otn:",
  "otn_resources_metadata",
  "&outputFormat=csv&CQL_FILTER=strMatches(node,'ACT')=true"
) |>
  URLencode() |>
  read.csv()

otn_metadata <- data.frame(
  names = flatten_names(otn_metadata$shortname)
)

exact_matches <- merge(
  matos_projects,
  otn_metadata
)

test_that("merge doesn't match multiple OTN projects.", {
  expect_equal(
    length(unique(exact_matches$names)),
    nrow(exact_matches)
  )
})

otn_dangler <- otn_metadata[!otn_metadata$names %in%
                              exact_matches$names,]
matos_dangler <- matos_projects[!matos_projects$names %in%
                                  exact_matches$names,]

otn_in_matos <- fuzzy_match_fun(otn_dangler, matos_dangler)
matos_in_otn <- fuzzy_match_fun(matos_dangler, otn_dangler)


test_that("agrep doesn't match multiple OTN projects.", {
  expect_false(
    any(sapply(otn_in_matos, length) > 1)
  )
  expect_false(
    any(sapply(matos_in_otn, length) > 1)
  )
})



test_that("no error in listing projects", {
  # skip test on Runiverse
  skip_on_Runiverse()

  expect_no_error({
    list_projects()
  })
})


test_that("returns correct classes", {
  # skip test on Runiverse
  skip_on_Runiverse()

  projects <- list_projects()

  expect_s3_class(projects, "data.frame")
  expect_type(projects$name, "character")
  expect_type(projects$number, "double")
  expect_type(projects$url, "character")
})
