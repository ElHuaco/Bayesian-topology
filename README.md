# Bayesian-topology
R implementation of the K2 algorithm for learning the topology of Bayesian belief networks.

## TODO:
2. Add return of relative probability for a structure, test best structure with
random order of the nodes.
3. Use monte carlo sampling with a known joint distribution of many variables to check that we return the highest relative probability to the correct structure.
5. Implement the same with ``bnstruct`` and compare relative probabilities found,
as well as computation time.
