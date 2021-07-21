library(httr)
library(jsonlite)

opendatacalgary_url <- "https://data.calgary.ca/data.json"

response <- httr::GET(opendatacalgary_url)
http_type(response)

data_json <- jsonlite::fromJSON(httr::content(response, "text"))
data_df <- tibble::as_tibble(data_json$dataset)


list_discovery <- function(path) {
  url <- modify_url("https://api.github.com", path = path)
  GET(url)
}



testme <- GET("http://api.us.socrata.com/api/catalog/v1?attribution=City%20of%20Chicago")

testme2 <- RSocrata::ls.socrata("http://api.us.socrata.com/api/")

github_api <- function(path) {
  url <- modify_url("https://api.github.com", path = path)

  resp <- GET(url, ua)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  if (status_code(resp) != 200) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "github_api"
  )
}
