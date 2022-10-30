#' Loops the instance's user directory
#'
#' @param instance character, URL of the Mastodon server
#' @param from integer (default NULL) offset. If NULL, the whole directory is parsed
#' @param limit integer (default 40). How many profiles to fetch in one request.
#' @return data.frame with user information
#' @export
get_instance_users <- function(instance, from=NULL, limit=40) {
	
	if(is.null(from)) {
		n_users <- get_instance_stats(instance)[1,"user_count"]
		slices <- seq(0, n_users, by=limit)
		ans <- lapply(
			slices, 
			function(x) get_instance_users(instance, from=x, limit=limit-1)
		) 
	} else {
		message("trying ", instance, ", offset=", from, "...")
		ans <- try({
			resp <- httr::GET(paste0(
				instance, "/api/v1/directory?offset=", from, "&limit=", limit
			)) |> httr::content()
			one_field <- function(y) paste0(
				strip_html(y[["name"]]), "=", strip_html(y[["value"]])
			) 
			ans <- lapply(resp, function(x) data.frame(
				username= x$username, acct=x$acct, display_name=x$display_name,
				locked=x$locked, bot=x$bot, created_at=x$created_at,
				note=strip_html(x$note),
				followers_count=x$followers_count, following_count=x$following_count,
				statuses_count=x$statuses_count, last_status_at=x$last_status_at,
				fields=paste(sapply(x$fields, one_field), collapse="; ")
			))
			# browser()
		}, silent=TRUE)
		if(inherits(ans, "try-error")) ans <- data.frame()
	}
	ans <- do.call(rbind, ans)
	return(ans)
}
