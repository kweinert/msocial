#' Get Information on a Mastodon Server
#'
#' @param instance URL of the Mastodon Server. Not vectorized!
#' @param verbose boolean (default TRUE) print diagnostic messages
#' @return data.frame with Server information
#' @export
get_instance_stats <- function(instance, verbose=TRUE) {
	stopifnot(length(instance)==1)
	if(verbose) message("trying ", instance, "...")
	ans <- try({
		resp <- httr::GET(paste0(instance, "/api/v1/instance")) |> httr::content()
		data.frame(
			instance=instance,
			registration=resp$registration,
			user_count=resp$stats$user_count,
			status_count=resp$stats$status_count,
			country_code=get_instance_countrycode(instance)			
		)
	}, silent=!verbose)
	if(inherits(ans, "try-error")) ans <- data.frame()
	return(ans)
}

