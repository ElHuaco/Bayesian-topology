# just gives back the counts of the unique possibilities
alpha <- function(D, i, pi){
    

    if(!i %in% pi){
        pi <- c(pi, i)
    }
    
    return(as.data.frame(table(D[,pi]))$Freq)
}


# computes Nij for any given parent nodes also dont matter if node itself is given or not
# int is the current node
# looks complicated because if the dataframe has only one line it turns to a vector 
# so some format changing had to be done
Nij <- function(D, int, pi){
    
    pi <- sort(pi)
    pi_new <- pi[!pi %in% int]
    j <- unique(D[,pi[!pi %in% int]])
    N <- c()
        

    if(length(pi) == 1 && pi == int){
        return(length(D[,pi]))
    }
    else if (length(pi) == 1 && pi != int) {
       pi <- sort(c(pi, int))
    }
    
    
    if(NCOL(j) == 1){
        j <- as.data.frame(j)
    }
    
    for(i in (1:nrow(j))){

        s <- as.numeric(j[i,])
        c <- D
        cnt <- c()
        
        for(l in 1:length(pi_new)){
            
            c <- D[which(c[,pi[l]] == s[l]),]#[,pi[l]:length(D)]

            if(l == length(pi_new)){
                N <- c(N, nrow(c))
            
            }
        }
    }
    
    return(N)
}

# return the the probability function f(i, pi) 
# D is the Dataset
# i is the current node
# pi is the possibile parent nodes, it doesnt matter if you give the node itself to it or not 
# example : {x1, x2} = {x1}  if we are on node x2
probability <- function(D, i, pi){  

        pi <- unique(pi)

        q <- length(unique(D[,pi]))
        j <- unique(D[,pi])
        r <- length(unique(D[,i]))

        P1 <- prod(factorial(r - 1)/(factorial(Nij(D, i, pi) + r - 1)))
        P2 <-  prod(factorial(alpha(D, i, pi)))


    return (P1 * P2)
}

# need to be done
k2 <- function(N, order, u, D){
    for(i in 0:N){
        
    }
}