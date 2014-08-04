fp_factory <- function() {
  BaseURL <- "https://www.googleapis.com/plus/v1/"
  People <- "people/"
  Activities <- "/activities/public?maxResults=100"
  KeyParam <- "&key="
  function(x, TotalFetches = 1) {
    if (!is.null(x$GetPosts())) {
      return(invisible(x))
    }
    posts <- list()
    demographics <- list(name = x$name, section = x$section)
    NextPageToken <- NULL
    for (i in 1:TotalFetches) {
      url <- paste0(BaseURL,
                    People,
                    RCurl::curlEscape(x$gplusid),
                    Activities,
                    NextPageToken,
                    KeyParam,
                    x$GetAPIKey())
      this.res <- rjson::fromJSON(RCurl::getURL(url))
      posts <- c(posts, this.res[["items"]])
      if (is.null(this.res[["nextPageToken"]])) {
        x$SetPosts(posts)
        return(invisible(x))
      } else {
        NextPageToken <- paste0("&pageToken=", this.res[["nextPageToken"]])
      }      
    }
    x$SetPosts(posts)
    return(invisible(x))
  }
}

#' @export
fetch_posts <- function(gpobj, ...) UseMethod("fetch_posts")
#' @export
fetch_posts.gp <- fp_factory()
