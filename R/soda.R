soda_api <- function(url = get_default_url(), refresh = FALSE) {

  # Cache the result so rerunning will not result in repeated API queries
  cache_file <- file.path(tempdir(), "cache.rda")
  if (!refresh & file.exists(cache_file)) {
    result <- readRDS(cache_file)
  } else {

    # Query SODA API
    path <- "data.json"
    url <- httr::modify_url(url, path = path)

    response <- httr::GET(url, ua)
    if (httr::http_type(response) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }

    parsed <- jsonlite::fromJSON(
      httr::content(response, "text"),
      simplifyVector = FALSE
    )

    if (httr::status_code(response) != 200) {
      stop(
        sprintf(
          "SODA API request failed [%s]\n%s\n<%s>",
          httr::status_code(response),
          parsed$message,
          parsed$documentation_url
        ),
        call. = FALSE
      )
    }

    result <- structure(
      list(
        content = parsed,
        path = path,
        response = response
      ),
      class = "soda_api"
    )

    # Cache the result
    saveRDS(result, cache_file)
  }

  result

}

list_datasets <- function(...) {

  soda_response <- soda_api(...)

  parsed <- jsonlite::fromJSON(httr::content(
    soda_response$response, "text")
  )

  #TODO: Move all tidying code to separate function
  result <- tibble::as_tibble(parsed$dataset)

  # The result returns a dataframe that has dataframes embedded in it that need
  # to be fixed
  contact_point <- tibble::as_tibble(as.list(result$contactPoint))
  contact_point <- dplyr::rename_with(contact_point, ~ paste0("contact_", .x))
  publisher <- tibble::as_tibble(as.list(result$publisher))
  publisher <- dplyr::rename_with(publisher, ~ paste0("publisher_", .x))

  result <- dplyr::select(result, -c(contactPoint, publisher))
  result <- dplyr::bind_cols(result, contact_point, publisher)

  # The tibble also needs tidying
  result <- janitor::clean_names(result)

  # Missing values should be NA
  result <- dplyr::mutate(
    result,
    dplyr::across(
      tidyselect::vars_select_helpers$where(is.character),
      ~ replace(.x, .x == "NULL", NA)
    ),
    dplyr::across(
      tidyselect::vars_select_helpers$where(is.character),
      ~ replace(.x, .x == "", NA)
    ),
    keyword = purrr::map(keyword, ~ replace(.x, is.null(.x), NA))
  )

  # Clean column names in distribution
  result <- dplyr::mutate(
    result,
    distribution = purrr::map(distribution, janitor::clean_names)
  )

  # Dates should be of class date
  result <- dplyr::mutate(
    result,
    dplyr::across(
      c(issued, modified), clock::date_parse
    )
  )

  # Additional metadata is needed to make API in other functions cleaner
  result <- dplyr::mutate(
    result,
    id = stringr::str_remove(landing_page, paste0(get_default_url(), "d/"))
  )

  # The order of columns should be human friendly
  result <- dplyr::relocate(
    result,
    title,
    id,
    tags = keyword,
    categories = theme,
    description,
    issued,
    modified
  )

  result
}
