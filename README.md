
# talkr

<!-- badges: start -->
[![R-CMD-check](https://github.com/elpaco-escience/talkr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/elpaco-escience/talkr/actions/workflows/R-CMD-check.yaml)  [![codecov](https://codecov.io/github/elpaco-escience/talkr/graph/badge.svg?token=MTA2S1LLGH)](https://codecov.io/github/elpaco-escience/talkr)
<!-- badges: end -->

## Note

This package is under development, and not ready for use.
Interact with us in the [issues](https://github.com/elpaco-escience/talkr/issues) if you have any questions or suggestions.

## Overview

{talkr} offers a set of convenience functions for quality control, visualisation and analysis of conversational data. It provides support for at least two common needs of people working with conversational corpora: (1) quality control by offering rapid insights into the nature, timing, and quality of time-aligned annotations in a conversational corpus; and (2) compelling visualisations by offering a range of plotting functions that play well with ggplot and the tidyverse. 

Note: {talkr} plays well with its companion python package [`scikit-talk`](https://github.com/elpaco-escience/scikit-talk), which is focused more on processing conversational data and readying it for downstream analysis.

## Installation

You can install the development version of `talkr` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("elpaco-escience/talkr", build_vignettes = TRUE)
```

## Workflow

The main workflow for `talkr` is described in the workflow vignette.
You can access the vignette by running:

``` r
vignette("workflow", package = "talkr")
```

