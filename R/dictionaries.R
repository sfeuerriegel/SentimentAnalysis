#' Loads Harvard-IV dictionary into object
#' 
#' Loads Harvard-IV dictionary (as used in General Inquirer) into a standardized
#' dictionary object
#' @return object of class \code{\link{SentimentDictionary}}
#' @note Result is a list of stemmed words in lower case
#' @importFrom utils data
#' @export
"loadDictionaryGI" <- function() {
  DictionaryGI <- NULL # surpress note "no visible binding"
  data(DictionaryGI, envir=environment())
  return(SentimentDictionary(unique(tm::stemDocument(DictionaryGI$positive, language="english")), 
                             unique(tm::stemDocument(DictionaryGI$negative, language="english"))))
}

#' Loads Henry's finance-specific dictionary into object
#' 
#' Loads Henry's finance-specific dictionar into a standardized dictionary object
#' @return object of class \code{\link{SentimentDictionary}}
#' @note Result is a list of stemmed words in lower case
#' @importFrom utils data
#' @export
"loadDictionaryHE" <- function() {
  DictionaryHE <- NULL # surpress note "no visible binding"
  data(DictionaryHE, envir=environment())
  return(SentimentDictionary(unique(tm::stemDocument(DictionaryHE$positive, language="english")), 
                             unique(tm::stemDocument(DictionaryHE$negative, language="english"))))
}

#' Loads Loughran-McDonald dictionary into object
#' 
#' Loads Loughran-McDonald financial dictionary into a standardized dictionary 
#' object (here, categories positive and negative are considered)
#' 
#' @return object of class \code{\link{SentimentDictionary}}
#' @note Result is a list of stemmed words in lower case
#' @importFrom utils data
#' @export
"loadDictionaryLM" <- function() {
  DictionaryLM <- NULL # surpress note "no visible binding"
  data(DictionaryLM, envir=environment())
  return(SentimentDictionary(unique(tm::stemDocument(DictionaryLM$positive, language="english")), 
                             unique(tm::stemDocument(DictionaryLM$negative, language="english"))))
}

#' Loads uncertainty words from Loughran-McDonald into object
#' 
#' Loads uncertainty words from Loughran-McDonald into a standardized
#' dictionary object
#' 
#' @return object of class \code{\link{SentimentDictionary}}
#' @note Result is a list of stemmed words in lower case
#' @importFrom utils data
#' @export
"loadDictionaryLM_Uncertainty" <- function() {
  DictionaryLM <- NULL # suppress note "no visible binding"
  data(DictionaryLM, envir=environment())
  return(SentimentDictionary(unique(tm::stemDocument(DictionaryLM$uncertainty, language="english"))))
}

#' Loads polarity words from qdap package into object
#' 
#' Loads polarity words from data object \code{\link[qdapDictionaries]{key.pol}} 
#' which is by the package qdap. This is then converted into a standardized
#' dictionary object
#' 
#' @return object of class \code{\link{SentimentDictionary}}
#' @note Result is a list of stemmed words in lower case
#' @references Hu and Liu (2004). Mining Opinion Features in Customer Reviews. 
#' National Conference on Artificial Intelligence.
#' @source \url{https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html}
#' @export
"loadDictionaryQDAP" <- function() {
  x <- NULL # suppress note "no visible binding"
  y <- NULL # suppress note "no visible binding"
  return(SentimentDictionary(unique(tm::stemDocument(subset(qdapDictionaries::key.pol, y==+1, select=x)[[1]], language="english")),
                             unique(tm::stemDocument(subset(qdapDictionaries::key.pol, y==-1, select=x)[[1]], language="english"))))
}


"allDictionaries" <- function() {
  return(list("GI"=list("Harvard-IV General Inquirer", loadDictionaryGI()), 
              "HE"=list("Henry's Finance-Specific", loadDictionaryHE()), 
              "LM"=list("Loughran-McDonald", loadDictionaryLM()), 
              "UncertaintyLM"=list("Loughran-McDonald Uncertainty", loadDictionaryLM_Uncertainty()), 
              "QDAP"=list("QDAP", loadDictionaryQDAP())))
}