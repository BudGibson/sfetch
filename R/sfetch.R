gplus_key <- function() {
  Sys.getenv('GPLUS_KEY')
}

resp_json <- function(resp) {
  jsonlite::fromJSON(httr::content(resp, as = "text"), 
                     simplifyVector = FALSE)
}
