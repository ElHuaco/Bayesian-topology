# just gives back the counts of the unique possibilities
alpha <- function(D, i, pi){

    return(as.data.frame(table(D[,pi]))$Freq)
}

# == Nij ==
# computes Nij for any given parent nodes also dont matter if node itself is given or not
# List of parameters:
# - D: dataframe of joint probability distributions measured.
# - i: index of the node at which to compute Nij.
# - parent_choice: list of chosen parent nodes for this particular permutation.
# Return values:
# - Nij: sum of Nijk, as described by F. Cooper and Herskovits on the /docs/
Nij <- function(D, i, parent_choice){
    parent_choice <- sort(parent_choice)
	# looks complicated because if the dataframe has only one line it turns to
	# a vector so some format changing had to be done
    parent_choice_new <- parent_choice[!parent_choice %in% i]
    j <- unique(D[,parent_choice[!parent_choice %in% i]])
    N <- c()
    if(length(parent_choice) == 1 && parent_choice == i){
        return(length(D[,parent_choice]))
    }
    else if (length(parent_choice) == 1 && parent_choice != i) {
       parent_choice <- sort(c(parent_choice, i))
    }
    if(NCOL(j) == 1){
        j <- as.data.frame(j)
    }
    for(i in (1:nrow(j))){
        s <- as.numeric(j[i,])
        c <- D
        for(l in 1:length(parent_choice_new)){
			#[,parent_choice[l]:length(D)] instead?
            c <- D[which(c[,parent_choice[l]] == s[l]),]
            if(l == length(parent_choice_new)){
                N <- c(N, nrow(c))
            }
        }
    }
    return(N)
}

# == Probability Mass ==
# List of parameters:
# - D: dataframe of joint probability distributions measured.
# - i: index of the node at which we compute the probability mass.
# - parent_choice: list of chosen parent nodes for this particular permutation.
# Return values:
# - P1*P2: it's f(i, pi) as described by Prof. Ruiz on the /docs/
probability <- function(D, i, parent_choice){
	# Remove the node itself from the possible parent_choice
	if(!i %in% parent_choice){
		parent_choice <- c(parent_choice, i)
	}
    parent_choice <- sort(unique(parent_choice))
	# TODO: consider log optimization
    r <- length(unique(D[,i]))
    P1 <- prod(factorial(r - 1)/(factorial(Nij(D, i, parent_choice) + r - 1)))
	# TODO: this should be ∏ Nijk for each k
    P2 <-  prod(factorial(alpha(D, i, parent_choice)))
    return (P1 * P2)
}

# == K2 Algorithm ==
# List of parameters:
# - order: the first node is the expected parent and every subsequent node
#     treats the previous ones as possible parents.
# - u: maximum number of parents per node allowed.
# - D: dataframe of joint probability distributions measured.
# Return values:
# - parents: list of parents per node.
k2 <- function(order, u, D){
	#TODO: feed randomized order and select the best structure
	#  add relative probability of that structure as a return value in that case
    N <- length(D)
    parents <- rep(list(0), length(D))
	# The first node does not have parents.
	parents[[1]] = integer(0)
    for(i in 2:N){
		# Permutation of current possible parents.
		# Use min(i, u) because of max number of parents allowed.
		# TODO: consider adding parents one by one instead of permutations
		pii <- t(combn(order[1:i], min(i, u)))
		P_old <- 0
	    for(s in 1:nrow(pii)){
			# Computes probability for each possible parent permutation
	        P_new <- probability(D, i, as.numeric(pii[s,]))
	        if(P_new > P_old){
	            P_old <- P_new
	            parents[[i]] <- pii[s,][!pii[s,] %in% i]
	        }
	    }
    print(paste('Parents of Node ', i, ' : Node =', parents[[i]]))
    }
	return (parents)
}
