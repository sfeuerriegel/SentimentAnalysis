
#' Count words
#'
#' Function counts the words in each document
#' @param x A vector of characters, a \code{data.frame}, an object of type 
#' \code{\link[tm]{Corpus}}, \code{\link[tm]{TermDocumentMatrix}} or
#' \code{\link[tm]{DocumentTermMatrix}} 
#' @param aggregate A factor variable by which documents can be grouped. 
#' This helpful when joining e.g. news from the same day or move reviews
#' by the same author
#' @param removeStopwords Flag indicating whether to remove stopwords or not (default: yes)
#' @param language Language used for preprocessing operations (default: 
#' English)
#' @param ... Additional parameters passed to function for e.g. 
#' preprocessing 
#' @return  Result is a matrix with word counts for each document across
#' @examples
#' documents <- c("This is a test", "an one more")
#' 
#' # count words (without stopwords)
#' countWords(documents)
#' 
#' # count all words (including stopwords)
#' countWords(documents, removeStopwords=FALSE)
#' @rdname countWords
#' @export
countWords <- function(x, aggregate=NULL,
                       removeStopwords=TRUE, language="english", ...) {
  UseMethod("countWords", x)
}

#' @rdname countWords
#' @export
"countWords.Corpus" <- function(x, aggregate=NULL,
                                removeStopwords=TRUE, language="english", ...) {
  dtm <- toDocumentTermMatrix(x, language=language, removeStopwords=removeStopwords, weighting=function(x) tm::weightTf(x),
                              minWordLength=1)
  return(countWords(dtm, aggregate=aggregate, removeStopwords=removeStopwords, language=language, ...))
}

#' @rdname countWords
#' @export
"countWords.character" <- function(x, aggregate=NULL,
                                   removeStopwords=TRUE, language="english", ...) {
  corpus <- transformIntoCorpus(x)
  return(countWords(corpus, aggregate=aggregate, removeStopwords=removeStopwords, language=language, ...))
}

#' @rdname countWords
#' @export
"countWords.data.frame" <- function(x, aggregate=NULL,
                                    removeStopwords=TRUE, language="english", ...) {
  corpus <- transformIntoCorpus(x)
  return(countWords(corpus, aggregate=aggregate, removeStopwords=removeStopwords, language=language, ...))
}

#' @rdname countWords
#' @export
"countWords.TermDocumentMatrix" <- function(x, aggregate=NULL,
                                            removeStopwords=TRUE, language="english", ...) {
  return(countWords(t(x), aggregate=aggregate, removeStopwords=removeStopwords, language=language, ...))
}

#' @rdname countWords
#' @export
"countWords.DocumentTermMatrix" <- function(x, aggregate=NULL,
                                            removeStopwords=TRUE, language="english", ...) {
  wc <- analyzeSentiment(x, aggregate=aggregate,
                         rules=list("WordCount"=list(ruleWordCount)), 
                         removeStopwords=removeStopwords, language=language)
  return(wc)
}
                       