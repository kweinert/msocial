#' Get Token for Private Access
#'
#' @param instance URL of the Mastodon Server
#' @param user username
#' @param pass password
#' @return character, access_token
#' @export
get_token = function(instance, user, pass) {
	if(substring(instance, nchar(instance))!="/") instance <- paste0(instance, "/")
	
	# registration
	client <- httr::POST(paste0(instance, "api/v1/apps"), body = list(
		client_name = 'msocial_r_pkg',
		redirect_uris = 'urn:ietf:wg:oauth:2.0:oob', 
		scopes = 'read write follow'
	)) |> 
	httr::stop_for_status() |>
	httr::content()
	stopifnot("client_id" %in% names(client) & "client_secret" %in% names(client))

	# login
	resp <- httr::POST(paste0(instance, 'api/v1/oauth/token'), body=list(
		client_id = client$client_id,
		client_secret = client$client_secret, 
		grant_type = 'password',
		username = user, 
		password = pass, 
		scope = 'read write follow'
	)) |> 
	httr::stop_for_status() |>
	httr::content()
	stopifnot("access_token" %in% names(client))
	
	res$access_token
}
