# mlr3learners.template

This packages provides a template for adding new learners for [mlr3](https://mlr3.mlr-org.com).

Creating new learners is covered in section ["Adding new learners"](https://mlr3book.mlr-org.com/extending-learners.html) in the mlr3book.
This package serves as a starting point for learners to share with others.

## Instructions

This repository is a template repository to create a learner that aligns with existing mlr3 learners.
Perform the following tasks to create your learner:




1. Check your package by running `rcmdcheck::rcmdcheck()`
1. Check if your learner complies with the [mlr style guide](https://github.com/mlr-org/mlr3/wiki/Style-Guide).
1. Ensure that the CI builds complete successfully (via the "Actions" menu in the repo).

After your learner is accepted, it can be added to [mlr3learners.drat](https://github.com/mlr3learners/mlr3learners.drat), making it installabe via the canonical `install.packages()` function without the need to live on CRAN.

**!Important!**: Delete all instructions up to this point and just leave the part below.

# mlr3learners.\<package\>

<!-- badges: start -->

[![R CMD Check via {tic}](https://github.com/mlr3learners/survival/workflows/R%20CMD%20Check%20via%20{tic}/badge.svg?branch=master)](https://github.com/mlr3learners/survival/actions)
[![Parameter Check](https://github.com/mlr3learners/mlr3learners.survival/workflows/Parameter%20Check/badge.svg?branch=master)](https://github.com/mlr3learners/mlr3learners.survival/actions)
[![codecov](https://codecov.io/gh/mlr3learners/mlr3learners.survival/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr3learners/mlr3learners.survival)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)

<!-- badges: end -->

Adds Nelson-Aalen estimator (`survfit`) and Parametric survival models `<survreg>` from the {survival} package to {mlr3}.

Install the latest release of the package via

```r
install.packages("mlr3learners.survival")
```

by following the instructions in the [mlr3learners.drat README](https://github.com/mlr3learners/mlr3learners.drat).

Alternatively, you can install the latest version of {mlr3learners.survival} from Github with:

```r
remotes::install_github("mlr3learners/mlr3learners.survival")
```
