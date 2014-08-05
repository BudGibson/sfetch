#' @export
posts_GET <- function(gpobj, ...) UseMethod("posts_GET")
#' @export
posts_GET.gp <- function(gppo,
                         total_fetches = 1,
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
                  gppo$GetAPIKey())
    resp <- httr::GET(url = url, path = path)
    this.res <- rjson::fromJSON(httr::content(resp, as = "text"))
    posts <- c(posts, this.res[["items"]])
    if (is.null(this.res[["nextPageToken"]])) {
      gppo$SetPosts(posts)
      return(invisible(gppo))
    } else {
      pageToken <- paste0("&pageToken=", this.res[["nextPageToken"]])
    }      
  }
  gppo$SetPosts(posts)
  return(invisible(gppo))  
}
