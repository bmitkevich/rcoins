
whattomine.altminer  <- function() {
  ret <- GET("https://altminer.net/api/status",
             add_headers("User-Agent","Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36"))
  stop_for_status(ret)
  ret <- content(ret)
  ret
}
