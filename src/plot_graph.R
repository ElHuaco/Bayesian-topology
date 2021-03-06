# plots the graph for the predicted parents
plot_graph <- function(parents, title = NULL, var_names = NULL){
    source <- c()
    target <- c()
    for(i in 1:length(parents)){
        for(s in parents[[i]]){
            source <- c(source, as.character(s))
            target <- c(target, as.character(i))
            
        }
    }
    links <- t(rbind(source, target))
    network <- graph_from_edgelist(links, directed=TRUE)
	if (is.null(var_names) == FALSE) 
		network <- set_vertex_attr(network, "label", value = var_names)
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
	if (is.null(title))
		title("predicted graph of dataset", cex.main=2,col.main="white")
	else
		title(title, cex.main=2,col.main="white")
}
