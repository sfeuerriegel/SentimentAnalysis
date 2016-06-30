
<!-- README.md is generated from README.Rmd. Please edit that file -->
Sentiment Analysis
==================

[![Build Status](https://travis-ci.org/sfeuerriegel/SentimentAnalysis.svg?branch=master)](https://travis-ci.org/sfeuerriegel/SentimentAnalysis) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/SentimentAnalysis)](http://cran.r-project.org/package=SentimentAnalysis) [![Coverage Status](https://img.shields.io/codecov/c/github/sfeuerriegel/SentimentAnalysis/master.svg)](https://codecov.io/github/sfeuerriegel/SentimentAnalysis?branch=master)

**SentimentAnalysis** performs a **sentiment analysis** of textual contents in R. This implementation utilizes various existing dictionaries, such as General Inquirer, Harvard IV or Loughran-McDonald. Furthermore, it can also create customized dictionaries. The latter uses LASSO regularization as a statistical approach to select relevant terms based on an exogeneous response variable.

Overview
--------

The most important functions in **SentimentAnalysis** are:

-   Compute sentiment scores from contents stored in different formats with `analyzeSentiment()`.

-   If desired, convert the continuous scores to either binary sentiment classes (negative or positive) or tertiary directions (negative, neutral or positive). This conversion can be done with `convertToBinary()` or `convertToDirection()` respectively.

-   Compare the calculated sentiment socres with a baseline (i.e. a gold standard). Here, `compareToResponse()` performs a statistical evaluation, while `plotSentimentResponse()` enables a visual comparison.

-   Generate customized dictionaries with the help of `generateDictionary()` as part of an advanced analysis. However, this prerequisites a response variable (i.e. the baseline).

To see examples of these functions in use, check out the help pages, the demos and the vignette.

Installation
------------

Using the **devtools** package, you can easily install the latest development version of **SentimentAnalysis** with

``` r
install.packages("devtools")

# Option 1: download and install latest version from GitHub
devtools::install_github("sfeuerriegel/SentimentAnalysis")

# Option 2: install directly from bundled archive
# devtoos::install_local("SentimentAnalysis_1.0.0.tar.gz")
```

Notes:

-   In the case of option 2, you have to specify the path either to the directory of **SentimentAnalysis** or to the bundled archive **SentimentAnalysis\_1.0.0.tar.gz**

-   A CRAN version has not yet been released.

Usage
-----

This section shows the basic functionality of how to perform a sentiment analysis. First, load the corresponding package **SentimentAnalysis**.

``` r
library(SentimentAnalysis)
```

### Quick demonstration

This simple example shows how to perform a sentiment analysis of a single string. The result is a two-level factor with levels "positive" and "negative."

``` r

# Analyze a single string to obtain a binary response (positive / negative)
sentiment <- analyzeSentiment("Yeah, this was a great soccer game of the German team!")
convertToBinaryResponse(sentiment)$SentimentGI
#> [1] positive
#> Levels: negative positive
```

### Small example

The following demonstrates some of the functionality provided by **SentimentAnalysis**. It also shows its visualization and evaluation capabilities.

``` r
# Create a vector of strings
documents <- c("Wow, I really like the new light sabers!",
               "That book was excellent.",
               "R is a fantastic language.",
               "The service in this restaurant was miserable.",
               "This is neither positive or negative.",
               "The waiter forget about my a dessert -- what a poor service!")

# Analyze sentiment
sentiment <- analyzeSentiment(documents)

# Extract dictionary-based sentiment according to the Harvard-IV dictionary
sentiment$SentimentGI
#> [1]  0.3333333  0.5000000  0.5000000 -0.6666667  0.0000000 -0.6000000

# View sentiment direction (i.e. positive, neutral and negative)
convertToDirection(sentiment$SentimentGI)
#> [1] positive positive positive negative neutral  negative
#> Levels: negative neutral positive

response <- c(+1, +1, +1, -1, 0, -1)

# TODO: compareToResponse(sentiment, response)

# TODO: plotSentimentResponse(sentiment$SentimentGI, response)
```

Dictionary generation
---------------------

Research in finance and social sciences nowadays utilizes content analysis to understand human decisions in the face of textual materials. While content analysis has received great traction lately, the available tools are not yet living up to the needs of researchers. This package implements a novel approach named "\*\*dictionary generation" to study tone, sentiment and reception of textual materials.

The approach utilizes LASSO regularization to extract words from documents that statistically feature a positive and negative polarity. This immediately reveals manifold implications for practitioners, finance research and social sciences: researchers can use R to extract text components that are relevant for readers and test their hypothesis based on these.

-   Proellochs, Feuerriegel and Neumann (2015): Generating Domain-Specific Dictionaries Using Bayesian Learning, Proceedings of the 23rd European Conference on Information Systems (ECIS 2015), Muenster, Germany. [DOI: 10.2139/ssrn.2522884](http://dx.doi.org/10.2139/ssrn.2522884)

License
-------

**SentimentAnalysis** is released under the [MIT License](https://opensource.org/licenses/MIT)

Copyright (c) 2016 Stefan Feuerriegel & Nicolas Pröllochs
