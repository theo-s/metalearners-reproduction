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
