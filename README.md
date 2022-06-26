# Bayesian-topology
R implementation of the K2 algorithm for learning the topology of Bayesian belief networks.

## TODO:
0. Reimplement correctly K2
1. Fix add parent possibilities one by one:
	In a network of 4 nodes we want to check respectively
	- none for 1st
	- none, 1 for 2nd
	- none, 1, 2, (1,2) for 3rd
	- none, 1, 2, (1,2), 3, (1,3), (2,3), (1,2,3) for 4th
2. Add return of relative probability for a structure, test best structure with
random order of the nodes.
3. Use monte carlo sampling with a known joint distribution of many variables to check that we return the highest relative probability to the correct structure.
4. Compare computation time of normal implementation with implementation of log
probability which turns products into sums.
5. Implement the same with ``bnstruct`` and compare relative probabilities found,
as well as computation time.
