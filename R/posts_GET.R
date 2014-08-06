#' @include sfetch.R
#' @include splus_GET.R

#' @export
posts_GET <- function(gpobj, ...) UseMethod("posts_GET")
#' @export
posts_GET.gp <- function(gppo,
                         total_fetches = 1,
                         page_token = NULL, ...) {
  if (!is.null(gppo$GetPosts())) {
    return(invisible(gppo))
  }
  posts <- list()
  for (i in 1:total_fetches) {
    resp <- gplus_GET(user = gppo$gplusid, page_token = page_token, ...)
    this.json <- resp_json(resp)
    posts <- c(posts, this.json[["items"]])
    if (is.null(this.json[["nextPageToken"]])) {
      gppo$SetPosts(posts)
      return(invisible(gppo))
    } else {
      page_token <- paste0("&pageToken=", this.json[["nextPageToken"]])
    }      
  }
  gppo$SetPosts(posts)
  return(invisible(gppo))  
}
