get_instance_countrycode <- function(instance) {
	stopifnot(length(instance)==1) # not vectorized
	ip <- iptools::hostname_to_ip(gsub("^https://", "", instance))[[1]]
	if(length(ip)>1) ip <- ip[1]
	if(ip=="Not resolved") return(NA)
	fn <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
	rgeolocate::maxmind(ip, fn)[,"country_code"]
}
	