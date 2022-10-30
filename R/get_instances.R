get_instances <- function() {
	fn <- system.file("instances.txt", package="msocial")
	x <- readLines(fn)
	x <- Filter(function(y) grepl("^https", y), unique(x))
	x
}