# == K2_bnstruct ==
k2_bnstruct <- function(D, u, bootstrap = FALSE){
	net <- learn.network(D, bootstrap)
	return (net)
}
