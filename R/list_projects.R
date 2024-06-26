#' List MATOS projects
#'
#' This function scrapes the table found at \url{https://matos.asascience.com/project}
#' and combines it with project metadata stored on the
#' \href{https://members.oceantrack.org/geoserver/web/}{Ocean Tracking Network Geoserver}.
#' This table provides the full name of the project, collection code, MATOS
#' project number, MATOS project page URL, project status, full name, citation,
#' website, project type, area, and abstract. You do not need to log in
#' via \code{matos_login} or have any permissions to view/download this table.
#'
#' @param what What list of projects do you want returned: all projects ("all",
#'      default) or your projects ("mine")?
#' @param read_access If listing your projects, do you want to only list
#'      projects for which you have file-read permission? Defaults to TRUE,
#'      though there is significant speed up if switched to FALSE.
#' @param quiet Do you want to suppress messages regarding matched projects?
#'      Defaults to FALSE.
#' @param force Do you want to reset the cache and re-ping the database?
#'      Defaults to FALSE.
#' @param warn_multimatch Warn you if there have been multiple project matches?
#'      Defaults to TRUE.
#'
#' @export
#' @examplesIf all(skip_example_on_cran(), skip_example_on_runiverse())
#' # List all projects, the default:
#' list_projects()
#'
#' # List your projects (which may contain some for which you do not have read access):
#' list_projects("mine", read_access = F)
list_projects <- function(what = c("all", "mine"),
                          read_access = TRUE,
                          quiet = FALSE,
                          force = FALSE,
                          warn_multimatch = TRUE) {
  if (isTRUE(force)) {
    memoise::forget(list_projects_mem)
  }

  what <- match.arg(what)

  list_projects_mem(
    what = what,
    read_access = read_access,
    quiet = quiet,
    warn_multimatch = warn_multimatch
  )
}

#' Memoised list_projects function for internal use.
#'
#' @inheritParams list_projects
#'
#' @keywords internal
list_projects_mem <- function(
    what,
    read_access,
    quiet,
    warn_multimatch) {
  if (what == "all") {
    # Download and parse MATOS project page
    project_list <- httr::GET(
      "https://matos.asascience.com/project"
    )

    projects_info <- httr::content(project_list)
    projects_info <- rvest::html_node(projects_info, ".project_list")
    projects_info <- rvest::html_nodes(projects_info, "a")

    urls <- rvest::html_attr(projects_info, "href")

    projects <- data.frame(
      name = rvest::html_text(projects_info, trim = T),
      number = as.numeric(gsub(".*detail/", "", urls)),
      url = paste0(
        "https://matos.asascience.com",
        urls
      )
    )


    # Pull ACT metadata from OTN database as it's a bit faster
    otn_metadata <- paste0(
      "https://members.oceantrack.org/geoserver/otn/ows?service=WFS&",
      "version=1.0.0&request=GetFeature&typeName=otn:",
      "otn_resources_metadata",
      "&outputFormat=csv&CQL_FILTER=strMatches(node,'ACT')=true"
    ) |>
      URLencode() |>
      read.csv()


    # Merge MATOS and OTN data
    ## Flatten and remove special characters to aid in matching
    projects$match_names <- flatten_names(projects$name)
    otn_metadata$match_names <- flatten_names(otn_metadata$shortname)


    ## Match MATOS and OTN projects
    exact_matches <- merge(projects, otn_metadata,
      by = "match_names"
    )

    ### Warn if there are multiple matches
    if (length(unique(exact_matches$match_names)) != nrow(exact_matches) &
      isTRUE(warn_multimatch)) {
      warning("MATOS has exactly matched multiple OTN project names.")
    }



    ## Find which are left over from the OTN and MATOS data sets
    otn_dangler <- otn_metadata[!otn_metadata$shortname %in%
      exact_matches$shortname, ]
    matos_dangler <- projects[!projects$name %in% exact_matches$name, ]



    ## Check for projects within other project names
    otn_in_matos <- within_match(
      otn_dangler$match_names,
      matos_dangler$match_names
    )

    matos_in_otn <- within_match(
      matos_dangler$match_names,
      otn_dangler$match_names
    )

    ## Create keys
    if (length(otn_in_matos) > 0 | length(matos_in_otn) > 0) {
      otn_in_matos <- data.frame(
        matos = unlist(otn_in_matos, use.names = F),
        otn = names(otn_in_matos)
      )
      matos_in_otn <- data.frame(
        matos = names(matos_in_otn),
        otn = unlist(matos_in_otn, use.names = F)
      )

      within_matches <- merge(otn_in_matos, matos_in_otn, all = T)

      ## Select metadata of within matches
      otn_match <- merge(
        otn_metadata, within_matches,
        by.x = "match_names", by.y = "otn"
      )
      matos_match <- merge(
        projects, within_matches,
        by.x = "match_names", by.y = "matos"
      )


      within_matches <- merge(
        matos_match, otn_match,
        by.x = c("otn", "match_names"),
        by.y = c("match_names", "matos")
      )
    } else {
      within_matches <- data.frame(
        name = character(),
        shortname = character()
      )
    }




    ## Find which are left over from the OTN and MATOS data sets
    otn_dangler <- otn_metadata[!otn_metadata$shortname %in%
      c(
        exact_matches$shortname,
        within_matches$shortname
      ), ]
    matos_dangler <- projects[!projects$name %in%
      c(
        exact_matches$name,
        within_matches$name
      ), ]




    ## Fuzzy match OTN names with MATOS names and vice versa
    otn_in_matos <- fuzzy_match(
      otn_dangler$match_names,
      matos_dangler$match_names
    )
    matos_in_otn <- fuzzy_match(
      matos_dangler$match_names,
      otn_dangler$match_names
    )

    ## Create keys
    if (length(otn_in_matos) > 0 | length(matos_in_otn) > 0) {
      otn_in_matos <- data.frame(
        matos = unlist(otn_in_matos, use.names = F),
        otn = names(otn_in_matos)
      )
      matos_in_otn <- data.frame(
        matos = names(matos_in_otn),
        otn = unlist(matos_in_otn, use.names = F)
      )

      ## Merge matches
      fuzzy_matches <- merge(otn_in_matos, matos_in_otn, all = T)

      ## Select metadata of fuzzy matches
      otn_match <- merge(
        otn_metadata, fuzzy_matches,
        by.x = "match_names", by.y = "otn"
      )
      matos_match <- merge(
        projects, fuzzy_matches,
        by.x = "match_names", by.y = "matos"
      )

      ## Merge keys
      fuzzy_matches <- merge(
        matos_match, otn_match,
        by.x = c("otn", "match_names"),
        by.y = c("match_names", "matos")
      )
    } else {
      fuzzy_matches <- data.frame(
        name = character(),
        shortname = character()
      )
    }


    ## Find which are left over from the OTN and MATOS data sets
    otn_dangler <- otn_metadata[!otn_metadata$shortname %in%
      c(
        exact_matches$shortname,
        within_matches$shortname,
        fuzzy_matches$shortname
      ), ]
    matos_dangler <- projects[!projects$name %in%
      c(
        exact_matches$name,
        within_matches$name,
        fuzzy_matches$name
      ), ]




    ## Combine matches
    matches <- Reduce(
      function(x, y) merge(x, y, all = T),
      list(
        exact_matches,
        within_matches[, !names(within_matches) == "otn"],
        fuzzy_matches[, !names(fuzzy_matches) == "otn"]
      )
    )

    ## Move names around
    projects <- merge(
      projects[, 1:3],
      matches[, !grepl("^match", names(matches))],
      all = T
    )

    missing_otn <- projects[is.na(projects$collectioncode), ]
    missing_act <- otn_metadata[!otn_metadata$collectioncode %in%
      projects$collectioncode, ]

    projects$collectioncode <- gsub("ACT\\.", "", projects$collectioncode)

    projects <- projects[, c(
      "name", "collectioncode", "number", "url",
      "status", "longname", "citation", "website",
      "collaborationtype", "locality", "abstract"
    )]


    if (nrow(missing_otn) != 0 & quiet == FALSE) {
      cli::cli_alert_info(
        list("These ACT projects were unable to be matched with OTN: {.val {missing_otn$name}}"),
        wrap = TRUE
      )
    }
    if (nrow(missing_act) != 0 & quiet == FALSE) {
      cli::cli_alert_info(
        list("These OTN projects were unable to be matched with ACT: {.val {missing_act$shortname}}"),
        wrap = TRUE
      )
    }
  }

  if (what == "mine") {
    projects <- list_my_projects(
      read_access = read_access,
      warn_multimatch = warn_multimatch
    )
  }

  projects
}

#' Flatten and remove special characters to aid in matching
#'
#' @param x name to flatten
#'
#' @keywords internal
flatten_names <- function(x) {
  hold <- tolower(x)
  gsub("[,\\(\\)_ /:'&\\.]|-", "", hold)
}



#' Check for projects within other project names
#'
#' @param a vector holding names that may be a part of `b`
#' @param b vector holding names that may encompass `a`
#'
#' @keywords internal
within_match <- function(a, b) {
  hold <- sapply(
    a, grep, b,
    value = TRUE,
    ignore.case = TRUE
  )

  match_lengths <- sapply(hold, length)


  # Error if there are multiple matches
  if (sum(match_lengths) != length(unique(unlist(hold)))) {
    stop("At least one project name is a subset of multiple other projects.")
  }


  hold[match_lengths == 1]
}



#' Use `agrep` for fuzzy matching
#'
#' @param a vector holding names that may be a part of `b` by changing less than
#'  25% of the characters
#' @param b vector holding names that may encompass some manipulation of `a`
#'
#' @keywords internal
fuzzy_match <- function(a, b) {
  hold <- sapply(
    a, agrep, b,
    max.distance = 0.25,
    value = TRUE,
    ignore.case = TRUE
  )

  match_lengths <- sapply(hold, length)


  # Error if there are multiple matches
  if (sum(match_lengths) != length(unique(unlist(hold)))) {
    stop(
      "At least one project name can be fuzzy-matched to multiple other projects."
    )
  }


  hold[sapply(hold, length) == 1]
}
