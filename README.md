# MAGMA
## Mathematica Automatized Galaxy Mass Analysis

[![license: GPLv3](https://img.shields.io/badge/license-GPLv3-brightgreen.svg)](https://github.com/FeynCalc/feyncalc/LICENSE.md)

**MAGMA** is a Wolfram Mathematica package that analyses rotation curve data and the baryonic mass distribution of galaxies. It can derive the best-fit parameters of a given model for each galaxy from a large sample. It can be used for testing dark matter halos or modified gravity.

The code will be available here shortly.

## Features

**MAGMA** is subdivided in the following parts:
* MAGMAfits - performs chi^2 minimization analysis for a given model.
* MAGMAplots - uses the output of MAGMAfits to generate plots for all the fitted galaxies with the best-fit parameters.
* MAGMAtable - generates a single table with diverse global data for each galaxy, including the results of MAGMAfits and also has a simple interface for importing [mBayes](https://github.com/valerio-marra/mBayes) results.
