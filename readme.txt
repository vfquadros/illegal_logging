This folder contains all data and code needed to reproduce results in my PSet. It contains 2 folders:

empirical facts: Contains inputs, code, and data exports that feed the model (tau, kappa, deforestation, etc)
model: contains the model, the functions/code I use to run it and also its exports.

In order to reproduce the results, you should run the codes in the following order (or you can just skip to model/01_codes/runs_counterfactuals.m):

Firstly, you need to load on the illegal_logging.Rproj, this will set the directory for the project.

1. tab_cleans_transport.R: gets data on the timber market. This will help define the geography later on.
2. plt_maps.R: Exports some data, defines the geography and plots the maps in the introduction.
3. export_matlab_variables: has the code to compute tau (fast marching algorithm), kappa and normalizes deforestation.

4. runs_counterfactuals.m: runs counterfactuals. You have to change phi manually to obtain the different results and also change the names of the outputs manually (Sorry, I know this is annoying).
	4.1 The functions used to compute the equilibrium (trade_shares.m, price_index.m and solve_equilibrium.m are in model/00_functions)

5. plt_results: plots the counterfactual maps.

