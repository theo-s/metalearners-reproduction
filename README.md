# Replication Repository

This repository contains the code needed to replicate the simulations in the paper: 
[Metalearners for estimating heterogeneous treatment effects using machine learning](https://www.pnas.org/content/116/10/4156).
The code folder contains several folders for replicating the simulations in 
different sections of the paper.

## Installation         

The package `causalToolbox` is required to run the replication code.
This can be installed using:

```
if (!require("devtools")){
    install.packages("devtools")
}
devtools::install_github("forestry-labs/causalToolbox")
```

It is also required to install the packages 
[Rforestry](https://CRAN.R-project.org/package=Rforestry) and 
[dbarts](https://CRAN.R-project.org/package=dbarts).


## Running Simulations

In order to run the simulations, you can run the corresponding script, and optionally 
supply the maximum sample size to run, and the numbers of replications to run.
For example, to run Experiment 1: (unbalanced treatment assignment) with a 
maximum sample size of 20,000, and 10 Monte Carlo replications, you should run:

```
Rscript 1-Sim_SI.R --n 20000 --r 10
```

In order to plot the results, run:

```
Rscript 7-plot_results.R
```
This will plot the results for all the experiments run, and update the plots in the 
`figures` folder.
