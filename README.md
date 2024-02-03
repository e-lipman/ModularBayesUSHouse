# ModularBayesUSHouse

This repository produces the results and figures from the following manuscript:

ADD CITATION

## The `IdealPointsCompare` Submodule
The source code for the analysis is contained in a submodule called [IdealPointsCompare](<https://github.com/e-lipman/IdealPointsCompare/tree/11e6be83530b39dcfd18dc1bac1d410dc372154d>). 
To clone the reposity with the submodule, run the following:
```
git clone --recurse-submodules git@github.com:e-lipman/ModularBayesUSHouse.git
```

Documentation for the source code and data is contained in the README for the `IdealPointsCompare` repository

## Scripts
`run_all`

- Top level bash script to run all analyses and produce all figures. Global hyperparameters are set to small values to run very short test runs for a few sessions of the House. Parameters needed to replicate the results in the manuscript are indicated via comments in this script.

- Running the script for the full-length analyses requires parallelization at the level of congress and chain by modifying the for loops in `bash_scripts/*`. Runtimes for stage 1 are on the order of 12-24hrs. Runtimes for the longer full model runs are on the order of days. Stage 2 for the two-stage and cut models are much faster.

`bash_scripts/*`

- Scripts in this folder run individual analyses. These scripts do not need to be modified when changing the hyperparameters as hyperparameters are passed from the top level scrip

## Figures

