#' Count by domain
#'
#' Returns each domain and the count of assets owned by that domain.
#'
#' @export
list_discovery_domains <- function() {

  discovery_domain_urls <- get_discovery_domain_urls()
  discovery_json <- purrr::map(discovery_domain_urls, jsonlite::read_json)

  discovery_domains <- purrr::map(discovery_json, ~{
    purrr::pluck(.x, "results")
  })

  tibble::tibble(domain = discovery_domains) %>%
    tidyr::unnest_longer(domain) %>%
    tidyr::unnest_wider(domain) %>%
    dplyr::arrange(desc(count))

}

get_discovery_domain_urls <- function() {
  base_urls <- get_discovery_urls()
  u <- purrr::map(base_urls, httr::parse_url)
  domain_urls <- purrr::map(u, ~{
    httr::modify_url(.x, path = c(.x$path, "domains"))
  })
  domain_urls
}

#' Discovery API
#'
#' See https://socratadiscovery.docs.apiary.io/
discovery_api <- function(endpoint = c("us", "eu"), query = NULL) {

  base_url <- get_discovery_urls()[rlang::arg_match(endpoint)]

  # Query Discovery API
  url <- httr::modify_url(base_url, query = query)

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

  #TODO: the last asset_id should be stored somewhere here since that is needed
  # for deep scrolling
  result <- structure(
    list(
      content = parsed,
      query = query,
      response = response
    ),
    class = "discovery_api"
  )

  result

}

#' Query Discovery API
#'
#' See https://socratadiscovery.docs.apiary.io/
#'
#' @param offset The initial starting point for paging.
#' @param limit Either the maximum number of results to return (up to 10000) or
#' `NULL`. If an integer, the output will return at most the number of results
#' in `limit`, started from the `offset`. If `NULL`, the output will return all
#' results after the `offset`, without any `limit`.
#' @param ids The unique four-by-four identifier of an asset. Draft assets can
#' be identified by appending the four-by-four identifier with either `:draft`
#' for stories or an integer of the draft number, e.g. `:1`, for non-story
#' drafts. This parameter supports repeated usage.
#' @param domains A comma separated string of canonical domain names. This
#' parameter does support repeated usage.
#' @param names The case-insensitive name of an asset. The output will only
#' return exact matches. The `query` argument should be used for partial
#' matches. This parameter supports repeated usage.
#' @param catgories A single category. This parameter supports repeated usage.
#' @param tags A single tag. This parameter supports repeated usage.
#' @param types A comma separated string of asset types. Any of api, calendar,
#' chart, datalens, dataset, federated_href, file, filter, form, href, link,
#' map, measure, story, visualization. This parameter does not support
#' repeated usage.
#' @param attribution The case-sensitive name of the attributing entity.
#' @param license The case-sensitive license name.
#' @param query A string for full text search.
#' @param min_should_match The number or percent of words that must match in `query`.
#' @param parent_ids The unique identifier of an asset.
#' @param derived_from The unique identifier of an asset.
#' @param provenance The provenance, as either 'official' or 'community'.
#' @param owner The unique identifier of a user or a team.
#' @param column_names The name of a column.
#' @param visibility The visibility status, as either 'open' or 'internal'.
#' @param audience The audience, as either 'private', 'site' or 'public'.
#' @param published A "true" or "false" value, for published or unpublished assets.
#' @param approval_status An approval status as enumerated above.
#' @param explicitly_hidden A "true" or "false" value, for hidden or unhidden assets.
#' @param data_json_hidden A "true" or "false" value, for hidden or unhidden assets.
#' @param derived A "true" or "false" value, for derived or base assets.
#' @param order The sort order of the results. One of name, owner, dataset_id,
#' datatype, domain_category, createdAt, updatedAt, page_views_total,
#' page_views_last_month, page_views_last_week.
#'
#'
#'
#'
#' #TODO: Finish docs
#'
#' @export
discovery_query <- function(
  offset = 0,
  limit = 100, # up to 10000; maybe allow setting to null for pagination
  ids = NULL, #TODO: figure out how to use repeatedly
  domains = NULL,
  names = NULL,
  categories = NULL,
  tags = NULL,
  types = NULL, # parameter for this is not 'types' it's 'only'
  attribution = NULL,
  license = NULL,
  query = NULL, # two parameters: 'q' and 'min_should_match'
  min_should_match = NULL,
  parent_ids = NULL,
  derived_from = NULL,
  provenance = NULL,
  owner = NULL, # parameter for this is 'for_user'
  column_names = NULL, # this has two parameters
  visibility = NULL, # c(NULL, "open", "internal"),
  audience =  NULL, # c(NULL, "private", "site", "public"),
  published =  NULL, # c(NULL, "false", "true"),
  approval_status = NULL,
  explicitly_hidden =  NULL, # c(NULL, "false", "true"),
  data_json_hidden =  NULL, # c(NULL, "false", "true"),
  derived =  NULL, # c(NULL, "false", "true"),
  order = NULL
) {

  discovery_api(
    endpoint = "us",
    query = c(
      offset = offset,
      limit = limit,
      repeat_parameter(ids, "ids"),
      domains = domains,
      repeat_parameter(names, "names"),
      repeat_parameter(categories, "categories"),
      repeat_parameter(tags, "tags"),
      repeat_parameter(tags, "tags"),
      only = types,
      attribution = attribution,
      license = license,
      q = query,
      min_should_match = min_should_match,
      parent_ids = parent_ids,
      derived_from = derived_from,
      provenance = provenance,
      owner = owner,
      column_names = column_names,
      visibility = visibility,
      audience = audience,
      published = published,
      approval_status = approval_status,
      explicitly_hidden = explicitly_hidden,
      data_json_hidden = data_json_hidden,
      derived = derived,
      order = order
    )
  )

}

as_tibble.discovery_api <- function(x) {

  results <- purrr::pluck(x, "content", "results")

  tibble::tibble(domain = results) %>%
    tidyr::unnest_wider(domain) %>%
    tidyr::unnest_wider(resource, names_sep = "_") %>%
    tidyr::unnest_wider(resource_page_views) %>%
    tidyr::unnest_wider(metadata, names_sep = "_") %>%
    tidyr::unnest_wider(owner, names_sep = "_") %>%
    tidyr::unnest_wider(creator, names_sep = "_") %>%
    tidyr::nest(page_views = dplyr::starts_with("page_views")) %>%
    tidyr::nest(metadata = dplyr::starts_with("metadata")) %>%
    tidyr::nest(owner = dplyr::starts_with("owner")) %>%
    tidyr::nest(creator = dplyr::starts_with("creator")) %>%
    #FIXME: This puts all the resource columns into a single list column with a tibble
    # that has columns for each field. This is the result I want but the code feels
    # messy.
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("resource_columns"),
        ~ purrr::map(.x, ~ unlist(.x))
      )
    ) %>%
    dplyr::group_by(resource_id) %>%
    dplyr::mutate(
      resource_columns = purrr::map(
        dplyr::row_number(),
        ~ tibble::tibble(
          name = unlist(resource_columns_name),
          field_name = unlist(resource_columns_field_name),
          datatype = unlist(resource_columns_datatype),
          description = unlist(resource_columns_description)
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::starts_with("resource_columns"), resource_columns)

}
