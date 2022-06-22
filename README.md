# Bayesian-topology
R implementation of the K2 algorithm for learning the topology of Bayesian belief networks.
Part one is finished.
K2 takes as input the order of the nodes. That means the first one is the predicted parent and every node after tries the ones before as parents. The second input is the upper maximum number of parents that are possbible but that does not mean that there have to be this number of parents. The last input is the Dataframe. Which has the rows as cases and the coloumns as nodes.
For plotting call 'plot_graph(parent)' and the inpute is the list of parents youreceive from K2.
