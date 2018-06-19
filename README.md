# MAGMA
## Mathematica Automatized Galaxy Mass Analysis

[![license: GPLv3](https://img.shields.io/badge/license-GPLv3-brightgreen.svg)](https://github.com/davi-rodrigues/MAGMA/blob/master/LICENSE.txt)

**MAGMA** is a Wolfram Mathematica package that analyses rotation curve data and the baryonic mass distribution of galaxies. It can derive the best-fit parameters of a given model for each galaxy from a large sample (with a single human interaction, the complete sample can be analysed). It can be used for testing dark matter halos or modified gravity.

This software is free to use and modify (under license GPLv3). In case it is used for a publication, please cite [D.C. Rodrigues et al Nat.Astron. 2018](http://www.nature.com/articles/s41550-018-0498-9). In case of doubts, let me know. (davi.rodrigues@cosmo-ufes.org).


## MAGMA parts

**MAGMA** is subdivided in the following parts:
* MAGMAfits - performs chi-squared minimization analysis for a given model.
* MAGMAplots - uses the output of MAGMAfits to generate plots for all the fitted galaxies with the best-fit parameters.
* MAGMAtable - generates a single table with diverse global data for each galaxy, including the results of MAGMAfits and also has a simple interface for importing [mBayes](https://github.com/valerio-marra/mBayes) results.

## Other features

* Multiple chi-squared minimizations done in parallel for each galaxy, and in two runs (the second run refines the first one). All minimizations are based on the differential evolution optimization. 
* MAGMA was at first developed to work with the [SPARC](http://astroweb.cwru.edu/SPARC/) data, hence it can be straightforwardly used with it. It can also be used with any other galaxy sample of disk galaxies, as long as the input data follow the same format. 
* Speed. For usual models (like MOND, NFW profile, Burkert profile...), all the 175 SPARC galaxies can be fit in a couple of hours with a desktop computer (or in about one hour if the second run is skipped).
* Bayesian inference and mBayes compatibility. MAGMAfit exports data of each galaxy's chi-squared function and the best-fit values, which can be nicely imported by [mBayes](https://github.com/valerio-marra/mBayes). mBayes can use the best-fit from MAGMA to determine the starting parameter space region to be explored.
