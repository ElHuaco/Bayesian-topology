# == all_possible_values ==
# List of parameters:
# - nodes: list of nodes
# - v: possible values each node can take
# Return Values:
# - A dataframe with all possible combinations of node values
all_possible_values <- function(nodes, v) {
  npar <- rep(list(v), length(nodes))
  return(expand.grid(npar))
}

# == Nijk ==
# List of parameters:
# - D: dataframe of joint probability distributions measured.
# - parent_choice: list of chosen parent nodes for this particular permutation.
# Return Values:
# - A list Nijk, over k, as described by F. Cooper and Herskovits on the /docs/
Nijk <- function(D, i, parent_instantiation, parent_choice, node_values){
	result <- c()
	for (k in node_values){
		data_node_at_k <- D[which(D[,i] == k),]
		parent_cols = setkey(data.table(data_node_at_k[,parent_choice]))
		parent_vals = setkey(data.table(parent_instantiation))
		result <- c(result,
					length(unique(na.omit(parent_cols[parent_vals, which=TRUE]))))
	}
	return (result)
}

# == Probability Mass ==
# List of parameters:
# - D: dataframe of joint probability distributions measured.
# - i: index of the node at which we compute the probability mass.
# - parents: list of chosen parent nodes so far.
# Return values:
# - f(i, pi) as described by Ruiz on the /docs/
probability_mass <- function(D, i, parents){
	v <- unique(D[,i])
    r <- length(v)
	if (parents[[1]] == 0 && length(parents) == 1){
		#No parents -> j index is ignored
		Ni_k <- as.data.frame(table(D[,i]))$Freq
		Ni_ <- sum(Ni_k)
		P1 <- factorial(r-1)/factorial(Ni_ + r - 1)
		P2 <- prod(factorial(Ni_k)) #Product over k
		return (P1 * P2)
	}
	#Parent choice determines what possible values j can take
	phi <- all_possible_values(parents, v)
	result <- 1.0
	#Product over j
	for (row in 1:nrow(phi)){
		nijk <- Nijk(D, i, phi[row,], parents, v)
		result <- result * prod(factorial(nijk)) #Product over k
		result <- result * factorial(r - 1) / (factorial(sum(nijk) + r - 1))
	}
    return (result)
}

# == maximize_probability_mass ==
# List of parameters:
# - D: dataframe of joint probability distributions measured.
# - i: index of the node at which we compute the probability mass.
# - parents: list of chosen parent nodes so far.
# Return values:
# - result: dataframe of the node that maximized the probability, at its value.
maximize_probability_mass <- function(D, i, parents){
	best_node <- 0
	P_max <- 0.0
	for (node in 1:(i-1)){
		if (length(parents[parents==node]) != 0){
			next
		}
		P_new <- probability_mass(D, i, c(parents, node))
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
# - D: dataframe of joint probability distributions measured.
# Return values:
# - result: dataframe of parents per node, and structure relative probability.
k2 <- function(order, u, D, print_result = TRUE){
	D <- D[, order]
    N <- length(D)
    parents <- rep(list(0), N)
	relat_prob <- 1.0
    for(i in 2:N){
		P_old <- probability_mass(D, i, parents[[i]])
		parents[i] <- lapply(parents[i], function(x) {x[x!=0]})
		while (length(parents[[i]]) < u){
			results <- maximize_probability_mass(D, i, parents[[i]])
	        if (results$P_new > P_old){
	            P_old <- results$P_new
				parents[[i]] <- c(parents[[i]], results$z)
	        }
			else{
				break
			}
	    }
		relat_prob <- relat_prob * P_old
    }
	#Remove '0' from parents, we treat it as the empty set
	for (i in 1:N){
		parents[i] <- lapply(parents[i], function(x) {x[x!=0]})
	}
	#Apply order to results
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
	ordered_parents <- ordered_parents[order]
	#Print parents per node
	if (print_result){
		for (i in 1:N){
			cat(noquote(paste("Parents of node ", i, ":\t", end=" ")))
			cat(noquote(paste("", as.character(ordered_parents[[i]]), "",
								collapse=", ",sep="")))
			cat("\n")
		}
	}
	result <- list(ordered_parents, relat_prob)
	return (result)
}
