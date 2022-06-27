# == Log Probability Mass ==
# List of parameters:
# - D: dataframe of joint log_probability distributions measured.
# - i: index of the node at which we compute the log_probability mass.
# - parents: list of chosen parent nodes so far.
# Return values:
# - f(i, pi) as described by Ruiz on the /docs/
log_probability_mass <- function(D, i, parents){
	v <- unique(D[,i])
    r <- length(v)
	if (parents[[1]] == 0 && length(parents) == 1){
		#No parents -> j index is ignored
		Ni_k <- as.data.frame(table(D[,i]))$Freq
		P1 <- log_fact[r] - log_fact[sum(Ni_k) + r]
		P2 <- sum(log_fact[Ni_k + 1]) #Sum over k
		return (P1 + P2)
	}
	#Parent choice determines what possible values j can take
	phi <- as.data.frame(unique(D[,parents]))
	result <- 0.0
	#Product over j
	for (row in 1:nrow(phi)){
		nijk <- Nijk(D, i, phi[row,], parents, v)
		result <- result + sum(log_fact[nijk + 1]) #Sum over k
		result <- result + log_fact[r] - log_fact[sum(nijk) + r]
	}
    return (result)
}

# == maximize_log_probability_mass ==
# List of parameters:
# - D: dataframe of joint log_probability distributions measured.
# - i: index of the node at which we compute the log_probability mass.
# - parents: list of chosen parent nodes so far.
# Return values:
# - result: df of the node that maximized the log_probability, at its value.
maximize_log_probability_mass <- function(D, i, parents){
	best_node <- 0
	P_max <- -Inf
	for (node in 1:(i-1)){
		if (length(parents[parents==node]) != 0){
			next
		}
		P_new <- log_probability_mass(D, i, c(parents, node))
		if (P_new > P_max){
			P_max <- P_new
			best_node <- node
		}
	}
	return (data.frame(z = c(best_node), P_new = c(P_max)))
}

# == K2 Algorithm ==
# List of parameters:
# - order: the first node is the expected parent and every subsequent node
#     treats the previous ones as possible parents.
# - u: maximum number of parents per node allowed.
# - D: dataframe of joint log_probability distributions measured.
# Return values:
# - result: list of parents per node, and structure relative log_probability.
k2_log <- function(D, u, print_result = FALSE, order = NULL){
	if (is.null(order) == FALSE){
		D <- D[, order]
	}
    N <- length(D)
    parents <- rep(list(0), N)
	relat_log_prob <- 0.0
    for (i in 2:N){
		P_old <- log_probability_mass(D, i, parents[[i]])
		parents[i] <- lapply(parents[i], function(x) {x[x!=0]})
		while (length(parents[[i]]) < u){
			results <- maximize_log_probability_mass(D, i, parents[[i]])
	        if (results$P_new > P_old){
	            P_old <- results$P_new
				parents[[i]] <- c(parents[[i]], results$z)
	        }
			else{
				break
			}
	    }
		relat_log_prob <- relat_log_prob + P_old
    }
	#Remove '0' from parents, we treat it as the empty set
	for (i in 1:N){
		parents[i] <- lapply(parents[i], function(x) {x[x!=0]})
	}
	#Apply order to results
	if (is.null(order) == FALSE){
		ordered_parents <- rep(list(numeric(u)), N)
		for (i in 1:N){
			ordered_parents[i] <- lapply(parents[i],
										 function(x) {return (order[x])})
		}
		reverse_order <- inverse(as.word(order))
		order <- c()
		for (i in 1:size(reverse_order)){
			order <- c(order, reverse_order[[i]])
		}
		parents <- ordered_parents[order]
	}
	#Print parents per node
	if (print_result){
		for (i in 1:N){
			cat(noquote(paste("Parents of node ", i, ":\t", end=" ")))
			cat(noquote(paste("", as.character(parents[[i]]), "",
								collapse=", ",sep="")))
			cat("\n")
		}
	}
	result <- list(parents, relat_log_prob)
	return (result)
}
