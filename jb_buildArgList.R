jb_buildArgList <- function (track = NULL, follow = NULL, language = NULL, locations = NULL, 
          with = NULL, replies = NULL, oauth = NULL) 
{
  params <- list()
  if (!is.null(track)) 
    params[["track"]] <- paste(track, collapse = ",")
  if (!is.null(follow)) 
    params[["follow"]] <- paste(as.character(follow), collapse = ",")
  if (!is.null(locations)) 
    params[["locations"]] <- paste(as.character(locations), 
                                   collapse = ",")
  if (!is.null(language)) 
    params[["language"]] <- paste(as.character(language), 
                                  collapse = ",")
  if (!is.null(with)) 
    params[["with"]] <- paste(as.character(with), collapse = ",")
  if (!is.null(replies)) 
    params[["replies"]] <- paste(as.character(replies), collapse = ",")
  return(params)
}
<environment: namespace:streamR>