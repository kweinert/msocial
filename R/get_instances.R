#' Return a manually curated list of Mastodon instances
#'
#' @return character of URLs
#' @export
get_instances <- function() {
	fn <- system.file("extdata/instances.txt", package="msocial")
	x <- readLines(fn)
	x <- Filter(function(y) grepl("^https", y), unique(x))
	x
}
