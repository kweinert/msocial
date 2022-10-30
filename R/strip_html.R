#' Remove HTML tags from a snippet
#'
#' See also https://stackoverflow.com/a/34344957/216064
#'
#' @param x character
#' @return character without HTML tags
#' @export
strip_html <- function(x) ifelse(
	nchar(x)>0,
	rvest::html_text(rvest::read_html(charToRaw(x))),
	""
)