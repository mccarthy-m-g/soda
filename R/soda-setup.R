# https://github.com/ropensci/ckanr/blob/master/R/ckanr_settings.R
soda_setup <- function(url = "https://data.calgary.ca/") {
  #TODO: Should make sure there's a trailing slash on the URL
  Sys.setenv("SODA_DEFAULT_URL" = url)
  Sys.setenv("DISCOVERY_US_URL" = "https://api.us.socrata.com/api/catalog/v1")
  Sys.setenv("DISCOVERY_EU_URL" = "https://api.eu.socrata.com/api/catalog/v1")
}

get_default_url <- function() { Sys.getenv("SODA_DEFAULT_URL") }
get_us_url <- function() { Sys.getenv("DISCOVERY_US_URL") }
get_eu_url <- function() { Sys.getenv("DISCOVERY_EU_URL") }
get_discovery_urls <- function() { c(us = get_us_url(), eu = get_eu_url()) }

ua <- httr::user_agent("http://github.com/hadley/httr")
