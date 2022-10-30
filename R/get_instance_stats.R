

get_instance_stats <- function(instance, verbose=TRUE) {
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

