#' @export
gplus_GET <- function(user = "+BudGibson",
                      api_key = gplus_key(),
                      url = "https://www.googleapis.com",
                      path_start = "/plus/v1/people/",
                      path_end = "/activities/public",
                      page_token = NULL) {
  path <- paste0(path_start,
                 RCurl::curlEscape(user),
                 path_end,
                 "?maxResults=100",
                 page_token,
                 "&key=",
                 api_key)
  resp <- httr::GET(url = url, path = path)
  httr::stop_for_status(resp)
  resp
}
  