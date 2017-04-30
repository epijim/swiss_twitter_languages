jb_filterStream <- function (file.name = NULL, track = NULL, follow = NULL, locations = NULL, 
          language = NULL, timeout = 0, tweets = NULL, oauth = NULL, 
          verbose = TRUE) 
{
  if (!is.null(oauth)) {
    library(ROAuth)
  }
  open.in.memory <- FALSE
  if (all(is.null(c(track, follow, language, locations)))) {
    stop("No filter parameter was specified. At least one is necessary. \n    \t\tSee ?filterStream for more information about this error.")
  }
  if (all(is.null(c(track, follow, locations))) & !is.null(language)) {
    stop("Language parameter can only be used in combination with other filter parameters.")
  }
  if ((missing(file.name) || is.character(file.name) == FALSE)) {
    stop("The file where the tweets will be stored was not named properly.")
  }
  if (timeout < 0 || is.numeric(timeout) == FALSE || length(timeout) > 
      1) {
    stop("The specified time out was not properly formatted.")
  }
  if (is.null(oauth)) {
    stop("No authentication method was provided. \n   \t\tPlease use an OAuth token.")
  }
  if (!is.null(oauth)) {
    if (!inherits(oauth, "OAuth")) {
      stop("oauth argument must be of class OAuth")
    }
    if (!oauth$handshakeComplete) {
      stop("Oauth needs to complete its handshake. See ?filterStream.")
    }
  }
  params <- buildArgList(track, follow, language, locations, 
                         oauth = oauth)
  i <- 0
  if (!is.null(file.name)) {
    if (verbose == TRUE) 
      message("Capturing tweets...")
    if (nchar(file.name) == 0) {
      open.in.memory <- TRUE
      file.name <- tempfile()
    }
    conn <- file(description = file.name, open = "a")
    write.tweets <- function(x) {
      if (nchar(x) > 0) {
        i <<- i + 1
        writeLines(x, conn, sep = "")
      }
    }
    if (!is.null(tweets) && is.numeric(tweets) && tweets > 
        0) {
      write.tweets <- function(x) {
        if (i >= tweets) {
          break
        }
        if (nchar(x) > 0) {
          i <<- i + 1
          writeLines(x, conn, sep = "")
        }
      }
    }
  }
  init <- Sys.time()
  url <- "https://stream.twitter.com/1.1/statuses/filter.json"
  if (!is.null(oauth)) {
    output <- tryCatch(oauth$OAuthRequest(URL = url, params = params, 
                                          method = "POST", customHeader = NULL, timeout = timeout, 
                                          writefunction = write.tweets, cainfo = system.file("CurlSSL", 
                                                                                             "cacert.pem", package = "RCurl")), error = function(e) e)
  }
  if (!is.null(file.name)) {
    close(conn)
  }
  seconds <- round(as.numeric(difftime(Sys.time(), init, units = "secs")), 
                   0)
  if (open.in.memory == TRUE) {
    raw.tweets <- readLines(file.name, warn = FALSE, encoding = "UTF-8")
    if (verbose == TRUE) {
      message("Connection to Twitter stream was closed after ", 
              seconds, " seconds with up to ", length(raw.tweets), 
              " tweets downloaded.")
    }
    unlink(file.name)
    return(raw.tweets)
  }
  if (open.in.memory == FALSE) {
    if (verbose == TRUE) {
      message("Connection to Twitter stream was closed after ", 
              seconds, " seconds with up to ", i, " tweets downloaded.")
    }
  }
}