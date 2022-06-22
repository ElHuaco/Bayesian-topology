# just gives back the counts of the unique possibilities
alpha <- function(D, i, pi){

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

        if(!i %in% pi){
        pi <- c(pi, i)
        }

        pi <- sort(unique(pi))
        r <- length(unique(D[,i]))

        P1 <- prod(factorial(r - 1)/(factorial(Nij(D, i, pi) + r - 1)))
        P2 <-  prod(factorial(alpha(D, i, pi)))


    return (P1 * P2)
}


# order describes the order of the graph so the the first one is the expected parent and every node
# after tries the ones before as parents but only as much as possbile -> u upper limit of parents
k2 <- function(order, u, D){
    N <- length(D)
    parents <- rep(list(0), length(D))
    
    for(i in 1:N){

        if(i == 1){
        pii <- combn(order[1], 1) # first node -> expected parent node
        }

        else{
        pii <- t(combn(order[1:i], min(i, u))) # permuatation of all possibile parents for current node
        }                                      # min(i, u) because when there is only one possible parent you 
                                               # cant take more than this for the permutation
                                               

        P_old <- 0
        

        for(s in 1:nrow(pii)){

            P_new <- probability(D, i, as.numeric(pii[s,])) # computes probability for all parents

            if(P_new > P_old){

                P_old <- P_new
                parents[[i]] <- pii[s,][!pii[s,] %in% i]
            }
        }

        #print(paste('Parents of Node ', i, ' : Node =', parents[[i]]))
    }

    
    return (parents)
}
 
# plots the graph for the predicted parents
plot_graph <- function(parents){
    source <- c()
    target <- c()
    for(i in 1:length(parents)){
        for(s in parents[[i]]){
            source <- c(source, as.character(s))
            target <- c(target, as.character(i))
            
        }
    }
    links <- data.frame(source, target)
    
    network <- graph_from_data_frame(d=links, directed=T)
    par(bg="black")
    igraph::plot.igraph(network, 
                                layout=layout.fruchterman.reingold,
                                vertex.color = rgb(0.8,0.4,0.3,0.8),
                                vertex.label.font=5,
                                vertex.label.cex = 1.9,
                                vertex.frame.color = "white",                 
                                vertex.shape="circle",                        
                                vertex.size=30,                               
                                vertex.label.color="white", 
                                edge.width=3,
                                edge.color='white',
                                edge.arrow.size=1,
                                edge.arrow.width=1,
                                edge.curved=0.2)
          title("predicted graph of dataset", cex.main=2,col.main="white")
}