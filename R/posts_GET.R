#' @include sfetch.R
#' @export
posts_GET <- function(gpobj, ...) UseMethod("posts_GET")
#' @export
posts_GET.gp <- function(gppo,
                         total_fetches = 1,
                         api_key = gplus_key(),
                         url = "https://www.googleapis.com",
                         path_start = "/plus/v1/people/",
                         path_end = "/activities/public") {
  if (!is.null(gppo$GetPosts())) {
    return(invisible(gppo))
  }
  posts <- list()
  demographics <- list(name = gppo$name, section = gppo$section)
  pageToken <- NULL
  for (i in 1:total_fetches) {
    path <- paste0(path_start,
                  RCurl::curlEscape(gppo$gplusid),
                  path_end,
                  "?maxResults=100",
                  pageToken,
                  "&key=",
                  api_key)
    resp <- httr::GET(url = url, path = path)
    httr::stop_for_status(resp)
    this.json <- jsonlite::fromJSON(httr::content(resp, as = "text"), simplifyVector = FALSE)
    posts <- c(posts, this.json[["items"]])
    if (is.null(this.json[["nextPageToken"]])) {
      gppo$SetPosts(posts)
      return(invisible(gppo))
    } else {
      pageToken <- paste0("&pageToken=", this.json[["nextPageToken"]])
    }      
  }
  gppo$SetPosts(posts)
  return(invisible(gppo))  
}
