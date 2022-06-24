# Bayesian-topology
R implementation of the K2 algorithm for learning the topology of Bayesian belief networks.


K2 takes three arguments:
1. First, the order of the nodes. That means the first one is the predicted parent and every node after tries the ones before as parents.
2. The second argument is the upper maximum number of parents that are possbible but that does not mean that there have to be this number of parents.
3. The third is the Dataframe. Which has the rows as cases and the coloumns as nodes.

For plotting call 'plot_graph(parent)' and the inpute is the list of parents youreceive from K2.

## TODO:
- Test performance of our R implementation (accuracy?).
- Implement with ``bnstruct`` package, plot the graph and test the accuracy as well.
