
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/acnb/rSimLab.png?branch=master)](https://travis-ci.org/acnb/rSimLab) [![Coverage Status](https://img.shields.io/codecov/c/github/acnb/rSimLab/master.svg)](https://codecov.io/github/acnb/rSimLab?branch=master)

rSimLab
=======

A package for the simulation of measurements in laboratory medicine. Complex settings can be created from simple building blocks.

Example:

``` r
# General settings (here: samples per day)
setting <- simpleOverTime(365,500, 50)
# Analytes with distribution of true values.
ana <- analyte(setting) %>%
    ana_distrNorm(3.5 , .886)
# Measurements with imprecision and bias.
mm <- measurement(ana) %>%
    mm_precCharFunc(.067, 0.031) %>%
    mm_accFunc(0, .03)
# Execute simulation.
runSim(mm)
```

Please refer to the inline documentation for further examples.

This package is not on CRAN yet. To install please run

``` r
install.packages("devtools")
library(devtools)
install_github("acnb/rSimLab")
```
