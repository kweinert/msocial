strip_html <- function(x) ifelse(
	nchar(x)>0,
	rvest::html_text(rvest::read_html(charToRaw(x))),
	""
)