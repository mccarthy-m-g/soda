# ----
soda_setup(url = "https://data.calgary.ca/")

soda_response <- soda_api(refresh = TRUE)
datasets <- list_datasets()

soda_setup(url = "http://data.edmonton.ca/")

soda_response <- soda_api(refresh = TRUE)
datasets <- list_datasets()

soda_setup(url = "https://gnb.socrata.com/")

soda_response <- soda_api(refresh = TRUE)
datasets <- list_datasets()

dataset_resources <- datasets$distribution[[1]]
dataset_response <- httr::GET(dataset_resources$download_url[2])
dataset_parsed <- jsonlite::fromJSON(httr::content(
  dataset_response, "text")
)
dataset <- tibble::as_tibble(dataset_parsed)

# ----

soda_setup()

discovery_response <- discovery_api()


path <- "data.json"
url <- httr::modify_url(get_us_url(), path = path)


# This gives same results as...
example <- httr::GET("http://api.us.socrata.com/api/catalog/v1?ids=2z5v-ecg8")
parsed <- jsonlite::fromJSON(
  httr::content(example, "text"),
  simplifyVector = FALSE
)
# this.
example <- jsonlite::read_json("http://api.us.socrata.com/api/catalog/v1?ids=2z5v-ecg8")

# However, without an api query only this method works
example_2 <- jsonlite::read_json("http://api.us.socrata.com/api/catalog/v1")

# ----
soda_setup()

q3 <- discovery_query(ids = c("2z5v-ecg8", "ai64-dnh8"))
q3_tibble <- as_tibble.discovery_api(q3)

#TODO: clean up classification cols. Decide if this method of tidying is
# good or else try to find something better
testme <- q3_tibble %>%
  tidyr::unnest_wider(classification, names_sep = "_") %>%
  dplyr::mutate(
    dplyr::across(
      classification_domain_tags,
      ~ purrr::map(.x, ~ unlist(.x))
    )
  )




testme$resource_columns

#list_discovery_domains()
discovery_api(query = list(ids = "2z5v-ecg8"))

q1 <- discovery_query()
q2 <- discovery_query(ids = "2z5v-ecg8")


q4 <- discovery_query(categories = "Education")
q5 <- discovery_query(categories = c("Education", "Environment"))
